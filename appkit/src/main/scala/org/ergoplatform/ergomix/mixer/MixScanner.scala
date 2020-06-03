package org.ergoplatform.ergomix.mixer

import org.ergoplatform.ergomix.ErgoMix
import org.ergoplatform.ergomix.cli.ErgoMixCLIUtil
import org.ergoplatform.ergomix.mixer.Models.{FBox, FollowedMix, HBox}
import sigmastate.eval._

object MixScanner {

  private def getSpendingTx(boxId:String) = ErgoMixCLIUtil.usingClient{implicit ctx =>
    val explorer = new BlockExplorer
    explorer.getSpendingTxId(boxId).flatMap(explorer.getTransaction)
  }

  def followHalfMix(halfMixBoxId:String, currentRound:Int, masterSecret:BigInt):Seq[FollowedMix] = {
    val secret = new Wallet(masterSecret).getSecret(currentRound).bigInteger
    val gZ = ErgoMix.g.exp(secret)

    getSpendingTx(halfMixBoxId).map { tx =>
      tx.outboxes.flatMap{outbox =>
        outbox.mixBox match {
          case Some(Right(FBox(fullMixBoxId, r4, r5, `gZ`))) if r4.exp(secret) == r5 =>
            println(s"$currentRound.5:[halfMixBoxId:$halfMixBoxId]->[fullMixBoxId:$fullMixBoxId]")
            FollowedMix(currentRound, isAlice = true, halfMixBoxId, Some(fullMixBoxId)) +: followFullMix(fullMixBoxId, currentRound, masterSecret)
          case _ => Nil
        }
      }
    }.getOrElse(Nil)
  }

  def followFullMix(fullMixBoxId:String, currentRound:Int, masterSecret:BigInt):Seq[FollowedMix] = {
    val wallet = new Wallet(masterSecret)
    val nextRound = currentRound + 1
    val nextSecret = wallet.getSecret(nextRound)
    val gZ = ErgoMix.g.exp(nextSecret.bigInteger)
    getSpendingTx(fullMixBoxId).map{tx =>
      tx.outboxes.flatMap{outbox =>
        outbox.mixBox match {
          case Some(Left(HBox(halfMixBoxId, `gZ`))) => // spent as Alice
            println(s"$nextRound.0:[fullMixBoxId:$fullMixBoxId]->[halfMixBoxId:$halfMixBoxId]")
            val futureMixes = followHalfMix(halfMixBoxId, nextRound, masterSecret)
            if (futureMixes.isEmpty) Seq(FollowedMix(nextRound, isAlice = true, halfMixBoxId, None)) else futureMixes
          case Some(Right(FBox(nextFullMixBoxId, g4, `gZ`, g6))) => // spent as Bob
            println(s"$nextRound.0:[fullMixBoxId:$fullMixBoxId]->[fullMixBoxId:$nextFullMixBoxId]")
            val halfMixBoxId = tx.inboxes.find(_.isHalfMixBox).get.id
            FollowedMix(nextRound, isAlice = false, halfMixBoxId, Some(nextFullMixBoxId)) +: followFullMix(nextFullMixBoxId, nextRound, masterSecret)
          case _ => Nil
        }
      }
    }.getOrElse(Nil)
  }

  @deprecated("This takes a lot of time. Use followFullMix and followHalfMix", "1.0")
  def followBoxId(boxId:String, masterSecret:BigInt): Seq[FollowedMix] = {
    getSpendingTx(boxId).map{tx =>
      tx.outboxes.filter(_.amount == ErgoMix.mixAmount).flatMap{outBox =>
        followHalfMix(outBox.id, 0, masterSecret) ++ followFullMix(outBox.id, 0, masterSecret)
      }
    }.getOrElse(Nil)
  }

}
