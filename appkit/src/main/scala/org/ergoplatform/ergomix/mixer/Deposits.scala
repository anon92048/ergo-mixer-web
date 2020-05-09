package org.ergoplatform.ergomix.mixer

import org.ergoplatform.ergomix.ErgoMix
import org.ergoplatform.ergomix.cli.ErgoMixCLIUtil
import org.ergoplatform.ergomix.db.ScalaDB._
import org.ergoplatform.ergomix.mixer.Columns._
import org.ergoplatform.ergomix.mixer.Util.now

class Deposits(tables: Tables) {
  import tables._

  def processDeposits():Unit = {
    mixRequestsTable.select(depositAddressCol).where(depositCompletedCol === false).firstAsT[String].map{depositAddress =>
      // println(s"Trying to read deposits for depositAddress: $depositAddress")
      ErgoMixCLIUtil.usingClient{implicit ctx =>
        val explorer = new BlockExplorer
        val allBoxes = explorer.getUnspentBoxes(depositAddress)

        val knownIds = unspentDepositsTable.select(boxIdCol).where(addressCol === depositAddress).firstAsT[String] ++
          spentDepositsTable.select(boxIdCol).where(addressCol === depositAddress).firstAsT[String]

        allBoxes.filterNot(box => knownIds.contains(box.id)).map{box =>
          insertDeposit(depositAddress, box.amount, box.id)
          println(s"Successfully processed deposit of ${box.amount} with boxId ${box.id} for depositAddress $depositAddress")
        }
      }
    }
  }

  private implicit val insertReason = "Deposits.insertDeposit"

  def insertDeposit(address: String, amount:Long, boxId:String): String = {
    // does not check from block explorer. Should be used by experts only. Otherwise, the job will automatically insert deposits
    if (mixRequestsTable.exists(depositAddressCol === address)) {
      if (unspentDepositsTable.exists(boxIdCol === boxId)) throw new Exception(s"Deposit already exists")
      if (spentDepositsTable.exists(boxIdCol === boxId)) throw new Exception(s"Deposit already spent")
      tables.insertUnspentDeposit(address, boxId, amount, now)
      val currentSum = unspentDepositsTable.select(amountCol).where(addressCol === address).firstAsT[Long].sum
      val needed = ErgoMix.mixAmount + ErgoMix.feeAmount
      if (currentSum >= needed) {
        mixRequestsTable.update(depositCompletedCol <-- true).where(depositAddressCol === address)
        "deposit completed"
      } else s"${needed - currentSum} nanoErgs pending"
    } else throw new Exception(s"Address $address does not belong to this wallet")
  }

}