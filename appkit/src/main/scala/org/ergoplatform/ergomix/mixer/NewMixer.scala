package org.ergoplatform.ergomix.mixer

import org.ergoplatform.ergomix.ErgoMix
import org.ergoplatform.ergomix.cli.{Alice, Bob, ErgoMixCLIUtil}
import org.ergoplatform.ergomix.db.ScalaDB._
import org.ergoplatform.ergomix.mixer.Columns._
import org.ergoplatform.ergomix.mixer.Models.MixStatus.{Queued, Running}
import org.ergoplatform.ergomix.mixer.Models.{Deposit, MixRequest}
import org.ergoplatform.ergomix.mixer.Util.now
import org.ergoplatform.ergomix.mixer.ErgoMixerUtil._

class NewMixer (tables: Tables) {
  import tables._

  def getDeposits(address:String): List[Deposit] = unspentDepositsTable.selectStar.where(addressCol === address).as(Deposit(_))

  def processNewMixQueue():Unit = {
    val reqs = mixRequestsTable.select(
      mixReqCols :+ masterSecretCol:_*
    ).where(
      mixStatusCol === Queued.value,
      depositCompletedCol === true
    ).as(arr =>
      (MixRequest(arr), arr.last.asInstanceOf[BigDecimal].toBigInt) // Mix details along with master secret
    )

    if (reqs.nonEmpty) println(s"[NEW] Processing following ids")

    reqs.foreach{
      case (mr, _) => println(s"  > ${mr.id} depositAddress: ${mr.depositAddress}")
    }

    reqs.map {
      case (mixRequest, masterSecret) =>
        try {
          initiateMix(mixRequest, masterSecret)
        } catch {
          case a:Throwable =>
            println(s" [NEW:${mixRequest.id}] An error occurred. Stacktrace below")
            a.printStackTrace()
        }
    }
  }

  private implicit val insertReason = "NewMixer.initiateMix"

  private def initiateMix(mixRequest: MixRequest, masterSecret:BigInt) = ErgoMixCLIUtil.usingClient{ implicit ctx =>
    val id = mixRequest.id
    val depositAddress = mixRequest.depositAddress
    val depositsToUse = getDeposits(depositAddress)

    val avbl = depositsToUse.map(_.amount).sum
    val needed = ErgoMix.mixAmount + ErgoMix.feeAmount

    if (avbl < needed) { // should not happen because we are only considering completed deposits.
      throw new Exception(s"Insufficient funds. Needed $needed. Available $avbl")
    } else {
      // now we have enough balance, lets proceed to the first round of the queue ...
      // first mark Mix as running
      mixRequestsTable.update(mixStatusCol <-- Running.value).where(mixIdCol === mixRequest.id)

      // always try to initiate as Bob first, and if it fails, do as Alice
      val wallet = new Wallet(masterSecret) // initialize wallet
      val secret = wallet.getSecret(0) // secret for the entry round

      val dLogSecret = wallet.getSecret(-1).toString()
      val inputBoxIds = depositsToUse.map(_.boxId).toArray

      val currentTime = now

      val optHalfMixBoxId = getRandomValidBoxId(ErgoMixCLIUtil.getHalfMixBoxes.filter(box => box.amount == ErgoMix.mixAmount && box.registers.get("R4").isDefined).map(_.id))

      val (txId, isAlice) = if (optHalfMixBoxId.nonEmpty) {
        // half-mix box exists... behave as Bob
        val halfMixBoxId = optHalfMixBoxId.get
        val (fullMixTx, bit) = Bob.spendHalfMixBox(secret, halfMixBoxId, inputBoxIds, ErgoMix.feeAmount, depositAddress, Array(dLogSecret), broadCast = true)
        val (left, right) = fullMixTx.getFullMixBoxes
        val bobFullMixBox = if (bit) right else left
        tables.insertFullMix(mixRequest.id, round = 0, time = currentTime, halfMixBoxId, bobFullMixBox.id)
        println(s" [NEW:$id] --> Bob [halfMixBoxId:$halfMixBoxId, txId:${fullMixTx.tx.getId}]")
        (fullMixTx.tx.getId, false) // is not Alice
      } else {
        // half-mix box does not exist... behave as Alice
        val tx = Alice.createHalfMixBox(secret, inputBoxIds, ErgoMix.feeAmount, depositAddress, Array(dLogSecret), broadCast = true)
        tables.insertHalfMix(mixRequest.id, round = 0, time = currentTime, tx.getHalfMixBox.id, isSpent = false)
        println(s" [NEW:$id] --> Alice [txId:${tx.tx.getId}]")
        (tx.tx.getId, true) // is Alice
      }

      depositsToUse.map{d =>
        tables.insertSpentDeposit(d.address, d.boxId, d.amount, d.createdTime, txId, currentTime, id)
        unspentDepositsTable.deleteWhere(boxIdCol === d.boxId)
      }

      mixStateTable.insert(mixRequest.id, 0, isAlice)
      tables.insertMixStateHistory(mixRequest.id, round = 0, isAlice = isAlice, time = currentTime)

    }
  }

}
