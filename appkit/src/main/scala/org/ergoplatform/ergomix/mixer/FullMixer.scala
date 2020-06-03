package org.ergoplatform.ergomix.mixer

import java.util

import org.ergoplatform.appkit.ErgoToken
import org.ergoplatform.ergomix.{ErgoMix, Token}
import org.ergoplatform.ergomix.cli.{AliceOrBob, ErgoMixCLIUtil}
import org.ergoplatform.ergomix.db.ScalaDB._
import org.ergoplatform.ergomix.db.core.DataStructures.anyToAny
import org.ergoplatform.ergomix.mixer.Columns._
import org.ergoplatform.ergomix.mixer.ErgoMixerUtil._
import org.ergoplatform.ergomix.mixer.Models.MixStatus.{Complete, Running}
import org.ergoplatform.ergomix.mixer.Util.now

class FullMixer(tables: Tables) {
  import tables._

  def processFullMixQueue():Unit = {

    // Read our full mix boxes from the full mix table and perform the next step. If the number of rounds are completed, then the next step will be withdraw, otherwise the next step is remix
    // If the next stp is remix, then perform as follows: if there is already a full mix box existing, then behave as Bob and try to spend that. Otherwise behave as Alice and create a half mix box.
    val fullMixes = mixRequestsTable.select(
      mixIdCol, // no need to use "of" for the table where the select query is made from. (i.e., mixRequestsTable)
      numRoundsCol,
      withdrawAddressCol,
      masterSecretCol,
      isAliceCol of mixStateTable,
      fullMixBoxIdCol of fullMixTable,
      roundCol of mixStateTable,
      halfMixBoxIdCol of fullMixTable
    ).where(
      mixIdCol === (mixIdCol of mixStateTable), // no need to use "of" for the table where the select query is made from
      mixIdCol === (mixIdCol of fullMixTable),
      (roundCol of fullMixTable) === (roundCol of mixStateTable),
      mixStatusCol === Running.value
    ).as{arr =>
        val i = arr.toIterator
        (
          i.next.as[String], // mixId
          i.next.as[Int],  // max rounds
          i.next.as[String], // withdraw address
          i.next.as[BigDecimal].toBigInt(), // master secret
          i.next.as[Boolean], // isAlice
          i.next.as[String], // fullMixBoxId
          i.next.as[Int], // current round
          i.next.as[String]  // halfMixBoxId
        )
    }

    if (fullMixes.nonEmpty) println(s"[FULL] Processing following ids")

    fullMixes foreach (x => println(s"  > ${x._1}"))

    fullMixes.map{
      case (mixId, maxRounds, withdrawAddress, masterSecret, isAlice, fullMixBoxId, currentRound, halfMixBoxId) =>
        try {
          val optEmissionBoxId = spentFeeEmissionBoxTable.select(boxIdCol).where(mixIdCol === mixId, roundCol === currentRound).firstAsT[String].headOption
          processFullMix(mixId, maxRounds, withdrawAddress, masterSecret, isAlice, fullMixBoxId, currentRound, halfMixBoxId, optEmissionBoxId)
        } catch {
          case a:Throwable =>
            println(s" [FULL:$mixId ($currentRound)] An error occurred. Stacktrace below")
            a.printStackTrace()
        }
    }
  }

  private implicit val insertReason = "FullMixer.processFullMix"

  private def str(isAlice:Boolean) = if (isAlice) "Alice" else "Bob"
  
  private def processFullMix(mixId:String, maxRounds:Int, withdrawAddress:String, masterSecret:BigInt, isAlice:Boolean, fullMixBoxId:String, currentRound:Int, halfMixBoxId:String, optEmissionBoxId:Option[String]) = ErgoMixCLIUtil.usingClient{ implicit ctx =>
    val explorer = new BlockExplorer()

    // If all is ok then the following should be true:
    //  1. halfMixBoxId must have minConfirmations
    //  2. fullMixBoxId must be unspent
    // If either of these are violated then we have to do a rescan and repopulate the database
    // The violations can occur due to a fork

    val fullMixBoxConfirmations = explorer.getConfirmationsForBoxId(fullMixBoxId)

    if (fullMixBoxConfirmations >= minConfirmations) {
      // proceed only if full mix box is mature enough
      val currentTime = now
      explorer.getSpendingTxId(fullMixBoxId) match {
        case Some(_) => // spent, we need to fast-forward... by rescanning block-chain
          println(s" [FULL:$mixId ($currentRound) ${str(isAlice)}] [ERROR] Rescanning because full:$fullMixBoxId is spent")
          Thread.currentThread().getStackTrace foreach println
          insertForwardScan(mixId, currentTime, currentRound, isHalfMixTx = false, fullMixBoxId)
        case None => // not spent, good to go
          val wallet = new Wallet(masterSecret)
          val secret = wallet.getSecret(currentRound)
          if (currentRound >= maxRounds) { // mix done
            val tx = AliceOrBob.spendFullMixBox(isAlice, secret, fullMixBoxId, withdrawAddress, Array[String](), ErgoMix.feeAmount, withdrawAddress, broadCast = true)
            val txBytes = tx.toJson(false).getBytes("utf-16")
            tables.insertWithdraw(mixId, tx.getId, currentTime, fullMixBoxId, txBytes)
            mixRequestsTable.update(mixStatusCol <-- Complete.value).where(mixIdCol === mixId)
            println(s" [FULL:$mixId ($currentRound) ${str(isAlice)}] Withdraw tx:${tx.getId}")
          } else { // need to remix
            // Note that the emission box contract requires that there must always be a emission box as the output. This will only work if there is some change to be given back
            // Hence we only select those emission boxes which have at least twice the fee amount.
            val optFeeEmissionBoxId = getRandomValidBoxId(
              ErgoMixCLIUtil.getFeeEmissionBoxes.filter(box => box.spendingTxId.isEmpty && box.amount >= 2 * ErgoMix.feeAmount).map(_.id).filterNot(id => spentFeeEmissionBoxTable.exists(boxIdCol === id))
            )

            val currentTime = now

            if (optFeeEmissionBoxId.nonEmpty) { // proceed only if there is at least one fee emission box

              val feeEmissionBoxId = optFeeEmissionBoxId.get
              // store emission boxid in db to ensure we are not double spending same emission box in multiple iterations of the loop
              val nextRound = currentRound + 1
              val nextSecret = wallet.getSecret(nextRound)

              def nextAlice = {
                val halfMixTx = AliceOrBob.spendFullMixBox_RemixAsAlice(isAlice, secret, fullMixBoxId, nextSecret, feeEmissionBoxId)
                tables.insertHalfMix(mixId, nextRound, currentTime, halfMixTx.getHalfMixBox.id, isSpent = false)
                mixStateTable.update(roundCol <-- nextRound, isAliceCol <-- true).where(mixIdCol === mixId)
                tables.insertMixStateHistory(mixId, nextRound, isAlice = true, currentTime)
                println(s" [FULL:$mixId ($currentRound) ${str(isAlice)}] --> Alice [full:$fullMixBoxId, fee:$feeEmissionBoxId]")
                halfMixTx.tx.getId
              }

              def nextBob(halfMixBoxId: String) = {
                val (fullMixTx, bit) = AliceOrBob.spendFullMixBox_RemixAsBob(isAlice, secret, fullMixBoxId, nextSecret, halfMixBoxId, feeEmissionBoxId)
                val (left, right) = fullMixTx.getFullMixBoxes
                val bobFullMixBox = if (bit) right else left
                tables.insertFullMix(mixId, nextRound, currentTime, halfMixBoxId, bobFullMixBox.id)
                mixStateTable.update(roundCol <-- nextRound, isAliceCol <-- false).where(mixIdCol === mixId)
                tables.insertMixStateHistory(mixId, nextRound, isAlice = false, currentTime)
                println(s" [FULL:$mixId ($currentRound) ${str(isAlice)}] --> Bob [full:$fullMixBoxId, half:$halfMixBoxId, fee:$feeEmissionBoxId]")
                fullMixTx.tx.getId
              }

              val fullMixBoxTokens:Seq[Token] = ctx.getBoxesById(fullMixBoxId).head.getTokens

              val optHalfMixBoxId = getRandomValidBoxId(ErgoMixCLIUtil.getHalfMixBoxes.filter(_.tokens == fullMixBoxTokens).filterNot(box => fullMixTable.exists(halfMixBoxIdCol === box.id)).map(_.id))

              if (optHalfMixBoxId.isEmpty) {
                nextAlice
              } else {
                nextBob(optHalfMixBoxId.get)
              }
              spentFeeEmissionBoxTable.insert(mixId, nextRound, feeEmissionBoxId)
            } else {
              println(s" [FULL:$mixId ($currentRound) ${str(isAlice)}] No fee emission boxes")
            }
          }
      }
    } else {
      println(s" [FULL:$mixId ($currentRound) ${str(isAlice)}] Insufficient confirmations ($fullMixBoxConfirmations) [full:$fullMixBoxId]")
      if (fullMixBoxConfirmations == 0) { // 0 confirmations for fullMixBoxId
        // first check the fork condition. If the halfMixBoxId is not confirmed then there is a fork
        explorer.doesBoxExist(halfMixBoxId) match {
          case Some(false) =>
            // halfMixBoxId is no longer confirmed. This indicates a fork. We need to rescan
            println(s"  [FULL:$mixId ($currentRound) ${str(isAlice)}] [ERROR] Rescanning [half:$halfMixBoxId disappeared]")
            Thread.currentThread().getStackTrace foreach println
            insertBackwardScan(mixId, now, currentRound, isHalfMixTx = false, fullMixBoxId)
          case Some(true) =>
            if (isDoubleSpent(halfMixBoxId, fullMixBoxId)) {
              // the halfMixBox used in the fullMix has been spent, while the fullMixBox generated has zero confirmations.
              try {
                println(s"  [FULL:$mixId ($currentRound) ${str(isAlice)}] <-- Bob (undo). [full:$fullMixBoxId not spent while half:$halfMixBoxId spent]")
                undoMixStep(mixId, currentRound, fullMixTable)
              } catch {
                case a:Throwable =>
                  a.printStackTrace()
              }
            } else {
              optEmissionBoxId.map{ emissionBoxId =>
                if (isDoubleSpent(emissionBoxId, fullMixBoxId)) {
                  // the emissionBox used in the fullMix has been spent, while the fullMixBox generated has zero confirmations.
                  try {
                    println(s"  [FULL:$mixId ($currentRound) ${str(isAlice)}] <-- Bob (undo). [full:$fullMixBoxId not spent while fee:$emissionBoxId spent]")
                    undoMixStep(mixId, currentRound, fullMixTable)
                  } catch {
                    case a:Throwable =>
                      a.printStackTrace()
                  }
                }
              }
            }
          case _ =>
        }
      }
    }
  }

}
