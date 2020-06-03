package org.ergoplatform.ergomix.mixer

import org.ergoplatform.ergomix.cli.ErgoMixCLIUtil
import org.ergoplatform.ergomix.db.DBManager
import org.ergoplatform.ergomix.db.ScalaDB._
import org.ergoplatform.ergomix.db.config.DBConfig
import org.ergoplatform.ergomix.mixer.Models.{Deposit, MixHistory}
import org.ergoplatform.ergomix.mixer.Models.MixStatus.Queued

class Tables(config:DBConfig) {
  import Columns._
  implicit val dbConfig = config
  /*
   The need to undo any of the steps (described below) can arise due to any of the following reasons:
     (F) Fork
     (H) A half-mix spent by us (when behaving like Bob) is also spent by someone else and our transaction gets rejected
     (E) A fee emission box spent by us is also spent by someone else and our transaction gets rejected.

   The steps to be undone can be classified into any of the 5. The possible undo reasons are also given
   1. Entry as Alice, i.e., round 0 as Alice (F)
   2. Entry as Bob, i.e., round 0 as Bob (F, H)
   3. Remix as Alice (F, E)
   4. Remix as Bob (F, H, E)
   5. Withdraw (F)

   Since 1 and 5 need to be undone only in the case of a fork, which is rare (assuming minConfirmation is large enough), we will keep the task manual for now.
   Note that 5 is already implemented as undoWithdraw below.

   TODO: Check if undo for 1 is already handled in Alice undo case, if not implement it

   The case for 2, 3, 4 is implemented below. Since these transactions can be undone due to a double spend, they must be handed automatically via jobs

   TODO: Track spent boxes so that above undo operations (1 and 5) are done via a job in case of a fork

   We must have archive tables for any tables that are affected by undo:
    1. unspentDeposits (done)
    2. spentDeposits (done)
    3. mixStateHistory (different from mixState!) (done)
    4. fullMix (done)
    5. halfMix (done)
    6. withdraw (done)


  */

  // mix requests
  val mixReqCols = Seq(mixIdCol, amountCol, numRoundsCol, mixStatusCol, createdTimeCol, withdrawAddressCol, depositAddressCol, depositCompletedCol)

  val mixRequestsTable = Tab.withName("mixing_requests").withCols(mixReqCols :+ masterSecretCol:_*).withPriKey(mixIdCol)

  // stores unspent deposits
  val unspentDepositsTable = Tab.withName("unspent_deposits").withCols(addressCol, boxIdCol, amountCol, createdTimeCol).withPriKey(boxIdCol)
  private val unspentDepositsArchiveTable = Tab.withName("unspent_deposits_archived").withCols(addressCol, boxIdCol, amountCol, createdTimeCol, insertReasonCol).withPriKey()


  def insertUnspentDeposit(address:String, boxId:String, amount:Long, time:Long)(implicit insertReason:String) = {
    unspentDepositsTable.insert(address, boxId, amount, time)
    unspentDepositsArchiveTable.insert(address, boxId, amount, time, insertReason)
  }

  // stores spent deposits
  val spentDepositsTable = Tab.withName("spent_deposits").withCols(addressCol, boxIdCol, amountCol, createdTimeCol, txIdCol, spentTimeCol, purposeCol).withPriKey(boxIdCol)
  private val spentDepositsArchiveTable = Tab.withName("spent_deposits_archived").withCols(addressCol, boxIdCol, amountCol, createdTimeCol, txIdCol, spentTimeCol, purposeCol, insertReasonCol).withPriKey()

  def insertSpentDeposit(address: String, boxId:String, amount:Long, createdTime:Long, txId:String, spentTime:Long, purpose:String)(implicit insertReason:String) ={
    spentDepositsTable.insert(address, boxId, amount, createdTime, txId, spentTime, purpose)
    spentDepositsArchiveTable.insert(address, boxId, amount, createdTime, txId, spentTime, purpose, insertReason)
  }

  private def undoSpend(deposit: Deposit) = ErgoMixCLIUtil.usingClient{ implicit ctx =>
    if (unspentDepositsTable.exists(boxIdCol === deposit.boxId)) throw new Exception(s"Unspent deposit already exists ${deposit.boxId}")
    val explorer = new BlockExplorer()
    if (explorer.getBoxById(deposit.boxId).spendingTxId.isDefined) throw new Exception(s"Cannot undo spend for already spend box ${deposit.boxId}")
    unspentDepositsTable.insert(deposit.address, deposit.boxId, deposit.amount, deposit.createdTime)
    spentDepositsTable.deleteWhere(boxIdCol === deposit.boxId)
  }

  def undoSpends(depositAddress:String) = {
    spentDepositsTable.selectStar.where(addressCol === depositAddress).as(Deposit(_)).map{deposit =>
      println(s"[UndoDeposit ${deposit.address}]")
      try {
        undoSpend(deposit)
      } catch {
        case a:Throwable =>
          println(s" [UndoDeposit ${deposit.address}] Error "+a.getMessage)
      }
    }
  }

  // mixStateTable stores the current status of the mix (reqId, (current) roundCol and isAliceCol).
  // It only stores the CURRENT state, that is whenever a round is incremented, the roundCol is incremented and isAliceCol is updated
  // The complete history of mix is stored in a separate table below (mixStateHistoryTable), which is needed for undoing a mix step in case of a double spend by someone else
  val mixStateTable = Tab.withName("mix_state").withCols(
    mixIdCol,
    roundCol, // current round
    isAliceCol // is Alice in current round?
  ).withPriKey(mixIdCol)

  // halfMixTable contains the entire history of half mixes (for this user).
  // Whether a row corresponds to the CURRENT state is determined by mixStateTable.. that is, if a row in mixStateTable exists with a matching roundCol and mixIdCol
  // We will never delete from this table, hence composite primary key.
  // When some other user spends a half mix box owned by us, then we update the isSpentCol to true and also add a corresponding entry in the fullMixTable for our full-mix box
  // For undoing, we will simply delete the row that was added to this table and update the mixStateTable accordingly
  val halfMixTable = Tab.withName("half_mix").withCols(
    mixIdCol, roundCol, createdTimeCol, halfMixBoxIdCol, isSpentCol
  ).withPriKey(mixIdCol, roundCol)

  private val halfMixArchiveTable = Tab.withName("half_mix_archived").withCols(
    mixIdCol, roundCol, createdTimeCol, halfMixBoxIdCol, isSpentCol, insertReasonCol
  ).withPriKey()

  def insertHalfMix(mixId:String, round:Int, time:Long, halfMixBoxId:String, isSpent:Boolean)(implicit insertReason:String) = {
    halfMixTable.insert(mixId, round, time, halfMixBoxId, isSpent)
    halfMixArchiveTable.insert(mixId, round, time, halfMixBoxId, isSpent, insertReason)
  }

  // fullMixTable contains the entire history of half mixes (for this user).
  // Whether a row corresponds to the CURRENT state is determined by mixStateTable.. that is, if a row in mixStateTable exists with a matching roundCol and mixIdCol
  // We will never delete from this table, hence composite primary key.
  // A row is added whenever we do a remix as Bob (i.e., consume someone else's half-mix box) to generate two full-mix boxes. The row will store the boxId of our full-mix box
  // Recall that when we do a (re)mix as Alice, a row is created in the halfMixTable as explained above.
  // A row is also added to the fullMixTable whenever someone else spends that half mix box. The row will store the boxId of our full-mix box
  // Thus, this table contains a row for both Alice and Bob roles, while the halfMixTable contains rows only for Alice roles
  // For undoing, we will simply delete the row that was added to this table and update the mixStateTable accordingly
  val fullMixTable = Tab.withName("full_mix").withCols(
    mixIdCol, roundCol, createdTimeCol, halfMixBoxIdCol, fullMixBoxIdCol // one belonging to us
  ).withPriKey(mixIdCol, roundCol)

  private val fullMixArchiveTable = Tab.withName("full_mix_archived").withCols(
    mixIdCol, roundCol, createdTimeCol, halfMixBoxIdCol, fullMixBoxIdCol, insertReasonCol // one belonging to us
  ).withPriKey()

  def insertFullMix(mixId:String, round:Int, time:Long, halfMixBoxId:String, fullMixBoxId:String)(implicit insertReason:String) = {
    fullMixTable.insert(mixId, round, time, halfMixBoxId, fullMixBoxId)
    fullMixArchiveTable.insert(mixId, round, time, halfMixBoxId, fullMixBoxId, insertReason)
  }
  // In case, there is a fork or a box is spent elsewhere, we would have to undo. Specifically, we would need to undo any of the following
  //  1. Bob entry (in case the half mix box is spent elsewhere)
  //  2. Reentry as alice (in case the fee emission box is spent elsewhere)
  //  3. Reentry as bob (in case either the fee emission box or the half mix box is spent elsewhere)
  //
  // Recall that the mixStateTable holds the CURRENT state of the mix, and a remix updates (roundCol, isAliceCol) for each new round.
  // The mixStateHistoryTable will store the entire history of the mix, not just the current state.
  // A new row must be inserted in this table whenever mixStateTable is updated
  //
  // To undo a round, we will follow the below strategy:
  //  1. Find the previous row in the history table
  //  2. Delete the corresponding row in fullMixTable and/or halfMixTable
  //  3. Update the corresponding row in the mixStateTable
  val mixStateHistoryTable = Tab.withName("mix_state_history").withCols(
    mixIdCol,
    roundCol, // current round
    isAliceCol, // is Alice in current round?
    createdTimeCol // time this row was entered
  ).withPriKey(mixIdCol, roundCol)

  private val mixStateHistoryArchiveTable = Tab.withName("mix_state_history_archived").withCols(
    mixIdCol,
    roundCol, // current round
    isAliceCol, // is Alice in current round?
    createdTimeCol, // time this row was entered
    insertReasonCol
  ).withPriKey()

  def insertMixStateHistory(mixId:String, round:Int, isAlice:Boolean, time:Long)(implicit insertReason:String) = {
    mixStateHistoryTable.insert(mixId, round, isAlice, time)
    mixStateHistoryArchiveTable.insert(mixId, round, isAlice, time, insertReason)
  }

  def undoMixStep(mixId:String, round:Int, mixTable:DBManager) = {
    val currRound = mixStateTable.select(roundCol).where(mixIdCol === mixId).firstAsT[Int].headOption.getOrElse(throw new Exception(s"No entry exists for $mixId in mixStateTable"))
    if (currRound != round) throw new Exception (s"Current round ($currRound) != undo round ($round)")

    val maxRound = mixStateHistoryTable.select(roundCol).where(mixIdCol === mixId).firstAsT[Int].max
    if (currRound != maxRound) throw new Exception (s"Current round ($currRound) != max round ($maxRound)")

    mixStateHistoryTable.deleteWhere(mixIdCol === mixId, roundCol === round)

    if (round == 0) {
      // delete from mixStateTable
      // delete from mixStateHistoryTable
      // set mixRequest as Queued

      mixStateTable.deleteWhere(mixIdCol === mixId)
      spentFeeEmissionBoxTable.deleteWhere(mixIdCol === mixId, roundCol === round)
      mixRequestsTable.update(mixStatusCol <-- Queued.value).where(mixIdCol === mixId)
      mixRequestsTable.select(depositAddressCol).where(mixIdCol === mixId).firstAsT[String].headOption.map(undoSpends)
    } else {
      val prevRound = round - 1
      mixStateHistoryTable.selectStar.where(mixIdCol === mixId, roundCol === prevRound).as(MixHistory(_)).headOption match {
        case Some(prevMixState) =>

          mixStateTable.update(
            roundCol <-- prevRound,
            isAliceCol <-- prevMixState.isAlice,
          ).where(mixIdCol === mixId)
          spentFeeEmissionBoxTable.deleteWhere(mixIdCol === mixId, roundCol === round)

        case _ => throw new Exception (s"No history found for previous round $prevRound and mixId $mixId")
      }
    }
    mixTable.deleteWhere(mixIdCol === mixId, roundCol === round)
  }

  val withdrawTable = Tab.withName("withdraw").withCols(mixIdCol, txIdCol, createdTimeCol, fullMixBoxIdCol, txCol).withPriKey(mixIdCol)

  private val withdrawArchiveTable = Tab.withName("withdraw_archived").withCols(mixIdCol, txIdCol, createdTimeCol, fullMixBoxIdCol, txCol, insertReasonCol).withPriKey()

  def insertWithdraw(mixId:String, txId:String, time:Long, fullMixBoxId:String, txBytes:Array[Byte])(implicit insertReason:String) = {
    withdrawTable.insert(mixId, txId, time, fullMixBoxId, txBytes)
    withdrawArchiveTable.insert(mixId, txId, time, fullMixBoxId, txBytes, insertReason)
  }

  // TODO: Add isAliceCol
  val spentFeeEmissionBoxTable = Tab.withName("emission_box").withCols(mixIdCol, roundCol, boxIdCol).withPriKey(boxIdCol)

  //mixId, time, round, goBackward, isHalfMixTx, mixBoxId
  val rescanTable = Tab.withName("rescan").withCols(
    mixIdCol, createdTimeCol, roundCol, goBackwardCol, isHalfMixTxCol, mixBoxIdCol
  ).withPriKey(mixIdCol)

  private val rescanArchiveTable = Tab.withName("rescan_archive").withCols(
    mixIdCol, createdTimeCol, roundCol, goBackwardCol, isHalfMixTxCol, mixBoxIdCol, insertReasonCol
  ).withPriKey()

  def insertForwardScan(mixId:String, time:Long, round:Int, isHalfMixTx:Boolean, mixBoxId:String)(implicit insertReason:String) = {
    insertPendingRescan(mixId, time, round, false, isHalfMixTx, mixBoxId, insertReason)
  }

  def insertBackwardScan(mixId:String, time:Long, round:Int, isHalfMixTx:Boolean, mixBoxId:String)(implicit insertReason:String) = {
    insertPendingRescan(mixId, time, round, true, isHalfMixTx, mixBoxId, insertReason)
  }

  private def insertPendingRescan(mixId:String, time:Long, round:Int, goBackward:Boolean, isHalfMixTx:Boolean, mixBoxId:String, insertReason:String) = {
    rescanTable.insert(mixId, time, round, goBackward, isHalfMixTx, mixBoxId)
    rescanArchiveTable.insert(mixId, time, round, goBackward, isHalfMixTx, mixBoxId, insertReason)
  }

}
