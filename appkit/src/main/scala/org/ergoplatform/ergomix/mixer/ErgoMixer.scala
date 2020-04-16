package org.ergoplatform.ergomix.mixer

import java.util.UUID

import org.ergoplatform.ergomix.cli._
import org.ergoplatform.ergomix.db.ScalaDB._
import org.ergoplatform.ergomix.mixer.Columns._
import org.ergoplatform.ergomix.mixer.Models.MixStatus.{Queued, Running}
import org.ergoplatform.ergomix.mixer.Models.{FullMix, HalfMix, Mix, MixHistory, MixRequest, MixState, Withdraw}
import org.ergoplatform.ergomix.mixer.Util._
import org.ergoplatform.ergomix.{ErgoMix, Util => EUtil}

/* ErgoMix is a single round of ZeroJoin with fee emission box, while ErgoMixer is multiple rounds of ErgoMix */
class ErgoMixer(tables:Tables) {
  import tables._
  Client.setClient("http://88.198.13.202:9053/", true, None)

  def newMixRequest(numRounds:Int, withdrawAddress:String) = {
    ErgoMixCLIUtil.usingClient { implicit ctx =>
      val util = new EUtil()
      try util.getAddress(withdrawAddress).script catch {
        case e:Throwable => throw new Exception("Invalid withdraw address")
      }
      // if here then address is valid
      val masterSecret = randBigInt
      val wallet = new Wallet(masterSecret)
      val depositSecret = wallet.getSecret(-1)
      val depositAddress = Carol.getProveDlogAddress(depositSecret)
      val mixId = UUID.randomUUID().toString
      mixRequestsTable.insert(mixId, ErgoMix.mixAmount, numRounds, Queued.value, now, withdrawAddress, depositAddress, false, masterSecret)
      s"Please deposit ${ErgoMix.feeAmount + ErgoMix.mixAmount} nanoErgs to $depositAddress"
    }
  }

  def getBalance = unspentDepositsTable.select(amountCol).firstAsT[Long].sum

  def getMixes = {
    mixRequestsTable.select(mixReqCols:_*).as(MixRequest(_)).map{req =>
      val mixState = mixStateTable.selectStar.where(mixIdCol === req.id).as(MixState(_)).headOption
      val halfMix = mixState.flatMap(state => halfMixTable.selectStar.where(mixIdCol === req.id, roundCol === state.round).as(HalfMix(_)).headOption)
      val fullMix = mixState.flatMap(state => fullMixTable.selectStar.where(mixIdCol === req.id, roundCol === state.round).as(FullMix(_)).headOption)
      val withdraw = withdrawTable.selectStar.where(mixIdCol === req.id).as(Withdraw(_)).headOption
      Mix(req, mixState, halfMix, fullMix, withdraw)
    }
  }

  def getActiveMixes = {
    getMixes.filter(_.mixRequest.mixStatus == Running)
  }

  def getPotentialBadAlice = {
    halfMixTable.selectStar.where(
      mixIdCol === mixIdCol.of(mixStateTable),
      roundCol === roundCol.of(mixStateTable),
      isSpentCol === false
    ).as(HalfMix(_)).filter(halfMix => ErgoMixCLIUtil.getSpendingTxId(halfMix.halfMixBoxId).isEmpty)
  }

  def getPotentialBadBob = {
    fullMixTable.selectStar.where(
      mixIdCol === mixIdCol.of(mixStateTable),
      roundCol === roundCol.of(mixStateTable),
      isAliceCol.of(mixStateTable) === false
    ).as(FullMix(_)).filter(fullMix => ErgoMixCLIUtil.getSpendingTxId(fullMix.fullMixBoxId).isEmpty)
  }

  def getMixStateHistoryTable = mixStateHistoryTable.selectStar.as(MixHistory(_))

  def getFullMixes(mixId:String) = {fullMixTable.selectStar.where(mixIdCol === mixId).as(FullMix(_))}

  def getHalfMixes(mixId:String) = {halfMixTable.selectStar.where(mixIdCol === mixId).as(HalfMix(_))}

  def getMixHistory(mixId:String) = {mixStateHistoryTable.selectStar.where(mixIdCol === mixId).as(MixHistory(_))}

  def decrementMixId(mixId:String, prevIsAlice:Boolean) = {
    val round = mixStateHistoryTable.select(roundCol).where(mixIdCol === mixId).firstAsT[Int].max
    val roundInMixState = mixStateTable.select(roundCol).where(mixIdCol === mixId).firstAsT[Int].headOption.getOrElse(throw new Exception(s"No entry found for mixId $mixId"))
    if (round != roundInMixState) throw new Exception(s"History mismatch. max round in history = $round. Current round $roundInMixState")
    if (round == 0) throw new Exception("Cannot decrement round 0")
    if (fullMixTable.exists(roundCol === round, mixIdCol === mixId)) throw new Exception(s"Round exists in full mix table")
    if (halfMixTable.exists(roundCol === round, mixIdCol === mixId)) throw new Exception(s"Round exists in half mix table")
    mixStateTable.update(roundCol <-- (round - 1), isAliceCol <-- prevIsAlice).where(mixIdCol === mixId)
    mixStateHistoryTable.deleteWhere(mixIdCol === mixId, roundCol === round)
  }
  /* General util methods, for testing or debugging

    def getEmissionBoxLog = spentFeeEmissionBoxTable.selectStar.as { a =>
      val mixId = a(0).asInstanceOf[String]
      val round = a(1).asInstanceOf[Int]
      val boxId = a(2).asInstanceOf[String]
      val txId = a(3).asInstanceOf[String]
      s"""{"mixId":"$mixId","round":$round,"boxId","$boxId","txId":"$txId"}"""
    }

    def clearEmissionBoxLog = spentFeeEmissionBoxTable.deleteAll

    def markAsIncomplete(mixId:String) = mixRequestsTable.update(mixStatusCol <-- Running.value).where(mixIdCol === mixId)

    def getWithdraws = withdrawTable.selectStar.as(Withdraw(_))

    def getMixRequests = mixRequestsTable.select(mixReqCols:_*).as(MixRequest(_))

    def getMixHistory = mixStateHistoryTable.selectStar.as(MixHistory(_))

    def getMixStates = mixStateTable.selectStar.as(MixState(_))

    def getHalfMixes = halfMixTable.selectStar.as(HalfMix(_))

    def getFullMixes = fullMixTable.selectStar.as(FullMix(_))
  */
}
