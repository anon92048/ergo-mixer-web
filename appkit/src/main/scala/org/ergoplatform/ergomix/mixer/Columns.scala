package org.ergoplatform.ergomix.mixer

import org.ergoplatform.ergomix.db.core.DataStructures.{BIGDEC, BLOB, BOOL, Col, UINT, ULONG, ULONGAuto, VARCHAR}

object Columns {

  val BIGSTR = VARCHAR(10000)
  val STR = VARCHAR(255)
  val BIGINT = BIGDEC(100, 0) // 100 digits which is approx 2^332, much higher than 2^256, which we need

  val addressCol = Col("address", BIGSTR) // max 10000 characters in an address allowed here
  val depositAddressCol = Col("deposit_address", STR) // deposit address will always be of type profeDlog, so 255 chars are enough
  val depositCompletedCol = Col("deposit_completed", BOOL) // indicates if the deposit is pending or completed
  val createdTimeCol = Col("created_time", ULONG) // unsigned long, epoch time millis

  val boxIdCol = Col("box_id", STR)
  val amountCol = Col("amount", ULONG)

  val txIdCol = Col("tx_id", STR)
  val purposeCol = Col("purpose", STR)
  val spentTimeCol = Col("spent_time", ULONG) // unsigned long, epoch time millis

  val numRoundsCol = Col("num_rounds", UINT)
  val mixIdCol = Col("mix_id", STR)
  val mixStatusCol = Col("status", STR)
  val masterSecretCol = Col("master_secret", BIGINT)
  val withdrawAddressCol = Col("withdraw_address", BIGSTR)

  val roundCol = Col("round", UINT)
  val isAliceCol = Col("is_alice", BOOL) // if not Alice then it is Bob

  val fullMixBoxIdCol = Col("full_mix_box_id", STR)
  val halfMixBoxIdCol = Col("half_mix_box_id", STR)
  val isSpentCol = Col("is_spent", BOOL)

  val txCol = Col("tx", BLOB)
}
