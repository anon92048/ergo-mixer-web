package org.ergoplatform.ergomix.mixer

import org.ergoplatform.ergomix.db.ScalaDB._
import org.ergoplatform.ergomix.db.config.DBConfig
import org.ergoplatform.ergomix.mixer.Models.MixHistory
import org.ergoplatform.ergomix.mixer.Models.MixStatus.Queued

class Tables(config:DBConfig) {
  import Columns._
  implicit val dbConfig = config
  /*
  archive for any tables that are affected by undo
  1. unspentDeposits (done)
  2. spentDeposits (done)
  3. mixStateHistory (different from mixState!) (done)
  4. fullMix (done)
  5. halfMix (done)
  6. withdraw (done)
   */

  // mix requests
  val mixReqCols = Seq(mixIdCol, amountCol, numRoundsCol, mixStatusCol, createdTimeCol, withdrawAddressCol, depositAddressCol, depositCompletedCol)

  val mixRequestsTable = Tab.withName("mixing_requests").withCols((mixReqCols :+ masterSecretCol):_*).withPriKey(mixIdCol)

  // stores unspent deposits
  val unspentDepositsTable = Tab.withName("unspent_deposits").withCols(addressCol, boxIdCol, amountCol, createdTimeCol).withPriKey(boxIdCol)
  val unspentDepositsArchiveTable = Tab.withName("unspent_deposits_archive").withCols(addressCol, boxIdCol, amountCol, createdTimeCol).withPriKey()
  // stores spent deposits
  val spentDepositsTable = Tab.withName("spent_deposits").withCols(addressCol, boxIdCol, amountCol, createdTimeCol, txIdCol, spentTimeCol, purposeCol).withPriKey(boxIdCol)
  val spentDepositsArchiveTable = Tab.withName("spent_deposits_archive").withCols(addressCol, boxIdCol, amountCol, createdTimeCol, txIdCol, spentTimeCol, purposeCol).withPriKey()

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

  val halfMixArchiveTable = Tab.withName("half_mix_Archive").withCols(
    mixIdCol, roundCol, createdTimeCol, halfMixBoxIdCol, isSpentCol
  ).withPriKey()

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

  val fullMixArchiveTable = Tab.withName("full_mix_archive").withCols(
    mixIdCol, roundCol, createdTimeCol, halfMixBoxIdCol, fullMixBoxIdCol // one belonging to us
  ).withPriKey()

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

  val mixStateHistoryArchiveTable = Tab.withName("mix_state_history_archive").withCols(
    mixIdCol,
    roundCol, // current round
    isAliceCol, // is Alice in current round?
    createdTimeCol // time this row was entered
  ).withPriKey()

  def undoMixState(mixId:String, round:Int) = {
    val currRound = mixStateTable.select(roundCol).where(mixIdCol === mixId).firstAsT[Int].headOption.getOrElse(throw new Exception(s"No entry exists for $mixId in mixStateTable"))
    if (currRound != round) throw new Exception (s"Current round ($currRound) != undo round ($round)")

    val maxRound = mixStateHistoryTable.select(roundCol).where(mixIdCol === mixId).firstAsT[Int].max
    if (currRound != maxRound) throw new Exception (s"Current round ($currRound) != max round ($maxRound)")

    if (round == 0) {
      // delete from mixStateTable
      // delete from mixStateHistoryTable
      // set mixRequest as Queued

      mixStateTable.deleteWhere(mixIdCol === mixId)
      mixStateHistoryTable.deleteWhere(mixIdCol === mixId, roundCol === round)
      spentFeeEmissionBoxTable.deleteWhere(mixIdCol === mixId, roundCol === round)
      mixRequestsTable.update(mixStatusCol <-- Queued.value).where(mixIdCol === mixId)
    } else {
      val prevRound = round - 1
      mixStateHistoryTable.selectStar.where(mixIdCol === mixId, roundCol === prevRound).as(MixHistory(_)).headOption match {
        case Some(prevMixState) =>

          mixStateTable.update(
            roundCol <-- prevRound,
            isAliceCol <-- prevMixState.isAlice,
          ).where(mixIdCol === mixId)
          mixStateHistoryTable.deleteWhere(mixIdCol === mixId, roundCol === round)
          spentFeeEmissionBoxTable.deleteWhere(mixIdCol === mixId, roundCol === round)

        case _ => throw new Exception (s"No history found for previous round $prevRound and mixId $mixId")
      }
    }
  }

  val withdrawTable = Tab.withName("withdraw").withCols(mixIdCol, txIdCol, createdTimeCol, fullMixBoxIdCol, txCol).withPriKey(mixIdCol)

  val withdrawArchiveTable = Tab.withName("withdraw_archive").withCols(mixIdCol, txIdCol, createdTimeCol, fullMixBoxIdCol, txCol).withPriKey()

  // TODO: Add isAliceCol
  val spentFeeEmissionBoxTable = Tab.withName("emission_box").withCols(mixIdCol, roundCol, boxIdCol).withPriKey(boxIdCol)
}
