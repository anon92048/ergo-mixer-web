package org.ergoplatform.ergomix.mixer

import java.security.SecureRandom

import org.ergoplatform.appkit.InputBox
import org.ergoplatform.ergomix.cli.{Alice, AliceOrBob, Bob, ErgoMixCLIUtil}
import org.ergoplatform.ergomix.db.ScalaDB._
import org.ergoplatform.ergomix.db.core.DataStructures.anyToAny
import org.ergoplatform.ergomix.mixer.Columns._
import org.ergoplatform.ergomix.mixer.Models.MixStatus.{Complete, Queued, Running}
import org.ergoplatform.ergomix.mixer.Models.{Deposit, FBox, HBox, MixRequest}
import org.ergoplatform.ergomix.mixer.Util.{now, randInt}
import org.ergoplatform.ergomix.{ErgoMix, FullMixBox}
import sigmastate.eval._

class ErgoMixerJobs(tables: Tables) {
  import tables._

  // The minimum number of confirmations for a current mix transaction before proceeding to next step
  val minConfirmations = 3

  def getDeposits(address:String): List[Deposit] = unspentDepositsTable.selectStar.where(addressCol === address).as(Deposit(_))

  def getSpentDeposits = spentDepositsTable.selectStar.as(Deposit(_))

  def getAllDeposits:List[Deposit] = unspentDepositsTable.selectStar.as(Deposit(_))

  def markAsQueued(mixId:String) = {
    if (mixStateTable.exists(mixIdCol === mixId)) throw new Exception("Mix already started")
    mixRequestsTable.update(mixStatusCol <-- Queued.value).where(mixIdCol === mixId)
  }

  def processNewMixQueue:Unit = {
    val reqs = mixRequestsTable.select(
      (mixReqCols :+ masterSecretCol):_*
    ).where(
      mixStatusCol === Queued.value,
      depositCompletedCol === true
    ).as(arr =>
      (MixRequest(arr), arr.last.asInstanceOf[BigDecimal].toBigInt) // Mix details along with master secret
    )

    println(s"[NewMixQueue] Processing following requests")

    reqs.foreach{
      case (mr, _) => println(s"id: ${mr.id}, depositAddress: ${mr.depositAddress}")
    }

    reqs.map {
      case (mixRequest, masterSecret) =>
        try {
          initiateMix(mixRequest, masterSecret)
        } catch {
          case a:Throwable =>
            println(s" [NewMixQueue] ERROR  Processing request ${mixRequest.id}: "+a.getMessage)
        }
    }
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

  private def initiateMix(mixRequest: MixRequest, masterSecret:BigInt) = ErgoMixCLIUtil.usingClient{ implicit ctx =>
    val id = mixRequest.id
    println(s"[NewMix $id] Trying to process new mix request with deposit address ${mixRequest.depositAddress}")
    val depositAddress = mixRequest.depositAddress
    val depositsToUse = getDeposits(depositAddress)
    println(s" [NewMix $id] Using following ${depositsToUse.size} deposits with deposit address ${mixRequest.depositAddress}")
    depositsToUse foreach (d => println(s"  > [NewMix $id] amount: ${d.amount}, boxId: ${d.boxId}"))

    val avbl = depositsToUse.map(_.amount).sum
    val needed = ErgoMix.mixAmount + ErgoMix.feeAmount

    if (avbl < needed) {
      println(s"   [NewMix $id] Available $avbl < Needed $needed. Aborting")
      throw new Exception(s"Insufficient funds. Needed $needed. Available $avbl")
    } else {
      println(s"   [NewMix $id] Available $avbl >= Needed $needed. Going ahead")
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
        val halfMixBoxId = optHalfMixBoxId.get
        println(s"    [NewMix $id] Behaving as Bob because half mix box exists")
        // half-mix box exists... behave as Bob

        println(s"     [NewMix $id] Using half mix box with id ${halfMixBoxId}")
        val (fullMixTx, bit) = Bob.spendHalfMixBox(secret, halfMixBoxId, inputBoxIds, ErgoMix.feeAmount, depositAddress, Array(dLogSecret), true)
        println(s"      [NewMix $id] Successfully created tx with txId ${fullMixTx.tx.getId}")
        val (left, right) = fullMixTx.getFullMixBoxes
        val bobFullMixBox = if (bit) right else left
        fullMixTable.insert(mixRequest.id, 0, currentTime, halfMixBoxId, bobFullMixBox.id)
        fullMixArchiveTable.insert(mixRequest.id, 0, currentTime, halfMixBoxId, bobFullMixBox.id)
        println(s"       [NewMix $id] Updated database with new mix status and full mix box Id ${bobFullMixBox.id}")
        (fullMixTx.tx.getId, false) // is not Alice
      } else {
        println(s"    [NewMix $id] Behaving as Alice because half mix box exists")
        // half-mix box does not exist... behave as Alice
        val tx = Alice.createHalfMixBox(secret, inputBoxIds, ErgoMix.feeAmount, depositAddress, Array(dLogSecret), true)
        println(s"     [NewMix $id] Successfully created tx with txId ${tx.tx.getId}")
        halfMixTable.insert(mixRequest.id, 0, currentTime, tx.getHalfMixBox.id, false /*is spent? (no) */)
        halfMixArchiveTable.insert(mixRequest.id, 0, currentTime, tx.getHalfMixBox.id, false /*is spent? (no) */)
        println(s"      [NewMix $id] Updated database with new mix status and half mix box Id ${tx.getHalfMixBox.id} and txId: ${tx.tx.getId}")
        (tx.tx.getId, true) // is Alice
      }

      depositsToUse.map{d =>
        spentDepositsTable.insert(d.address, d.boxId, d.amount, d.createdTime, txId, currentTime, id)
        spentDepositsArchiveTable.insert(d.address, d.boxId, d.amount, d.createdTime, txId, currentTime, id)
        unspentDepositsTable.deleteWhere(boxIdCol === d.boxId)
      }

      mixStateTable.insert(mixRequest.id, 0, isAlice)
      mixStateHistoryTable.insert(mixRequest.id, 0, isAlice, currentTime)
      mixStateHistoryArchiveTable.insert(mixRequest.id, 0, isAlice, currentTime)

    }
  }
  def processFullMixQueue:Unit = {

    println(s" Processing following full-mixes")

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
    ).as{
      case arr =>
        val i = arr.toIterator
        (
          i.next.as[String], // mixId
          i.next.as[Int],  // max rounds
          i.next.as[String], // withdraw address
          i.next.as[BigDecimal].toBigInt(), // master secret
          i.next.as[Boolean], // isAlice
          i.next.as[String], // fullMixBoxId
          i.next.as[Int], // current round
          i.next.as[String]
        )
    }

    fullMixes foreach (x => println(s"  > ${x._1}"))

    fullMixes.map{
      case (mixId, maxRounds, withdrawAddress, masterSecret, isAlice, fullMixBoxId, currentRound, halfMixBoxId) => // proceed only if full mix box is mature enough
        try {
          val optEmissionBoxId = spentFeeEmissionBoxTable.select(boxIdCol).where(mixIdCol === mixId, roundCol === currentRound).firstAsT[String].headOption
          processFullMix(mixId, maxRounds, withdrawAddress, masterSecret, isAlice, fullMixBoxId, currentRound, halfMixBoxId, optEmissionBoxId)
        } catch {
          case a:Throwable =>
            println(s" [FullMix ${mixId}] ERROR in currentRound: $currentRound: ${a.getMessage}")
        }
    }
  }

  private def isDoubleSpent(boxId:String, explorer: BlockExplorer, wrtBoxId:String): Boolean = {
    explorer.getSpendingTxId(boxId).flatMap { txId =>
      // boxId has been spent, while the fullMixBox generated has zero confirmations. Looks like box has been double-spent elsewhere
      explorer.getTransaction(txId).map{tx =>
        // to be sure, get the complete tx and check that none if its outputs are our fullMixBox
        !tx.outboxes.map(_.id).contains(wrtBoxId)
      }
    }
  }.getOrElse(false)

  def getRandomValidBoxId(origBoxIds:Seq[String]) = ErgoMixCLIUtil.usingClient{ implicit ctx =>
    val random = new SecureRandom()
    val boxIds = new scala.util.Random(random).shuffle(origBoxIds)
    boxIds.find{boxId =>
      try {
        ctx.getBoxesById(boxId)
        true
      } catch{
        case a:Throwable =>
          println(s"      Error reading boxId ${boxId}: "+a.getMessage)
          false
      }
    }
  }

  private def processFullMix(mixId:String, maxRounds:Int, withdrawAddress:String, masterSecret:BigInt, isAlice:Boolean, fullMixBoxId:String, currentRound:Int, halfMixBoxId:String, optEmissionBoxId:Option[String]) = ErgoMixCLIUtil.usingClient{ implicit ctx =>
    val explorer = new BlockExplorer()
    println(s" Checking confirmations (full mix) mixId: $mixId, isAlice: $isAlice, fullMixBoxId: $fullMixBoxId, currentRound: $currentRound, halfMixBoxId: $halfMixBoxId, emissionBoxId: ${optEmissionBoxId.getOrElse("none")}")
    val fullMixBoxConfirmations = explorer.getConfirmationsForBoxId(fullMixBoxId)
    if (fullMixBoxConfirmations >= minConfirmations) {
      // proceed only if full mix box is mature enough
      println(s"  Enough confirmations ($fullMixBoxConfirmations). Processing (full mix) mixId: $mixId, isAlice: $isAlice, fullMixBoxId: $fullMixBoxId, currentRound: $currentRound")

      val wallet = new Wallet(masterSecret)
      val secret = wallet.getSecret(currentRound)
      val currentTime = now
      if (currentRound >= maxRounds) { // mix done
        println(s"   Doing withdraw for mixId $mixId")
        val tx = AliceOrBob.spendFullMixBox(isAlice, secret, fullMixBoxId, withdrawAddress, Array[String](), ErgoMix.feeAmount, withdrawAddress, true)
        val txBytes = tx.toJson(false).getBytes("utf-16")
        withdrawTable.insert(mixId, tx.getId, currentTime, fullMixBoxId, txBytes)
        mixRequestsTable.update(mixStatusCol <-- Complete.value).where(mixIdCol === mixId)
        println(s"    Withdraw txId ${tx.getId}. Mix marked as complete: $mixId")
      } else { // need to remix
        println(s"   Doing remix for mixId $mixId")
        // Note that the emission box contract requires that there must always be a emission box as the output. This will only work if there is some change to be given back
        // Hence we only select those emission boxes which have at least twice the fee amount.
        val optFeeEmissionBoxId = getRandomValidBoxId(
          ErgoMixCLIUtil.getFeeEmissionBoxes.filter(box => box.spendingTxId.isEmpty && box.amount >= 2 * ErgoMix.feeAmount).map(_.id).filterNot(id => spentFeeEmissionBoxTable.exists(boxIdCol === id))
        )

        val currentTime = now

        if (optFeeEmissionBoxId.nonEmpty) { // proceed only if there is at least one fee emission box

          val feeEmissionBoxId = optFeeEmissionBoxId.get

          println(s"    Using fee emission box $feeEmissionBoxId when remixing mixId $mixId")

          // store emission boxid in db to ensure we are not double spending same emission box in multiple iterations of the loop
          val nextRound = currentRound + 1
          val nextSecret = wallet.getSecret(nextRound)

          def nextAlice = {
            println(s"      Remixing: isAlice: $isAlice, fullMixBoxId: $fullMixBoxId, feeEmissionBoxId: $feeEmissionBoxId")
            val halfMixTx = AliceOrBob.spendFullMixBox_RemixAsAlice(isAlice, secret, fullMixBoxId, nextSecret, feeEmissionBoxId)
            halfMixTable.insert(mixId, nextRound, currentTime, halfMixTx.getHalfMixBox.id, false)
            halfMixArchiveTable.insert(mixId, nextRound, currentTime, halfMixTx.getHalfMixBox.id, false)
            mixStateTable.update(roundCol <-- nextRound, isAliceCol <-- true).where(mixIdCol === mixId)
            mixStateHistoryTable.insert(mixId, nextRound, true, currentTime)
            mixStateHistoryArchiveTable.insert(mixId, nextRound, true, currentTime)
            halfMixTx.tx.getId
          }

          def nextBob(halfMixBoxId: String) = {
            println(s"      Remixing: isAlice: $isAlice, fullMixBoxId: $fullMixBoxId, halfMixBoxId: $halfMixBoxId, feeEmissionBoxId: $feeEmissionBoxId")
            val (fullMixTx, bit) = AliceOrBob.spendFullMixBox_RemixAsBob(isAlice, secret, fullMixBoxId, nextSecret, halfMixBoxId, feeEmissionBoxId)

            val (left, right) = fullMixTx.getFullMixBoxes
            val bobFullMixBox = if (bit) right else left
            fullMixTable.insert(mixId, nextRound, currentTime, halfMixBoxId, bobFullMixBox.id)
            fullMixArchiveTable.insert(mixId, nextRound, currentTime, halfMixBoxId, bobFullMixBox.id)
            mixStateTable.update(roundCol <-- nextRound, isAliceCol <-- false).where(mixIdCol === mixId)
            mixStateHistoryTable.insert(mixId, nextRound, false, currentTime)
            mixStateHistoryArchiveTable.insert(mixId, nextRound, false, currentTime)
            fullMixTx.tx.getId
          }

          val optHalfMixBoxId = getRandomValidBoxId(ErgoMixCLIUtil.getHalfMixBoxes.filterNot(box => fullMixTable.exists(halfMixBoxIdCol === box.id)).map(_.id))

          if (optHalfMixBoxId.isEmpty) {

            println(s"     Behaving as Alice when remixing mixId $mixId")

            nextAlice

          } else {

            println(s"     Behaving as Bob when remixing mixId $mixId")

            nextBob(optHalfMixBoxId.get)

          }
          spentFeeEmissionBoxTable.insert(mixId, nextRound, feeEmissionBoxId)
        } else {
          println("    No fee emission boxes available. Unable to remix. Please create some fee emission boxes")
        }
      }
    } else {
      println(s"  Insufficient confirmations $fullMixBoxConfirmations for (full mix) mixId: $mixId, isAlice: $isAlice, fullMixBoxId: $fullMixBoxId, currentRound: $currentRound")
      if (fullMixBoxConfirmations == 0) { // 0 confirmations for fullMixBoxId
        println(s"   Zero conf for (full mix) mixId: $mixId, isAlice: $isAlice, fullMixBoxId: $fullMixBoxId, currentRound: $currentRound")

        if (isDoubleSpent(halfMixBoxId, explorer, fullMixBoxId)) {
          println(s"    Zero conf for (full mix) mixId: $mixId, isAlice: $isAlice, fullMixBoxId: $fullMixBoxId, currentRound: $currentRound while halfMixBoxId $halfMixBoxId spent")
          // the halfMixBox used in the fullMix has been spent, while the fullMixBox generated has zero confirmations.
          try {
            undoBadBob(mixId, currentRound)
            println(s"     Undo success Bob for $mixId, currentRound: $currentRound (fullMixBoxId $fullMixBoxId not spent while halfMixBoxId $halfMixBoxId spent)")
          } catch {
            case a:Throwable =>
              println(s"     Undo ERROR Bob for $mixId, currentRound: $currentRound (fullMixBoxId $fullMixBoxId not spent while halfMixBoxId $halfMixBoxId spent)")
          }
        } else {
          optEmissionBoxId.map{ emissionBoxId =>
            if (isDoubleSpent(emissionBoxId, explorer, fullMixBoxId)) {
              println(s"    Zero conf for (full mix) mixId: $mixId, isAlice: $isAlice, fullMixBoxId: $fullMixBoxId, currentRound: $currentRound while emissionBoxId $emissionBoxId spent")
              // the emissionBox used in the fullMix has been spent, while the fullMixBox generated has zero confirmations.
              try {
                undoBadBob(mixId, currentRound)
                println(s"     Undo success Bob for $mixId, currentRound: $currentRound because fullMixBoxId $fullMixBoxId not spent while emissionBoxId $emissionBoxId spent")
              } catch {
                case a:Throwable =>
                  println(s"     Undo ERROR Bob for $mixId, currentRound: $currentRound because fullMixBoxId $fullMixBoxId not spent while emissionBoxId $emissionBoxId spent. ${a.getMessage}")
              }
            }
          }
        }
      }
    }
  }

  private def processHalfMix(mixId:String, currentRound:Int, halfMixBoxId:String, masterSecret:BigInt, optEmissionBoxId:Option[String]) = ErgoMixCLIUtil.usingClient{implicit ctx =>
    println(s" Attempting to processing Half-Mix queue for mixId: $mixId, currentRound: $currentRound, halfMixBoxId: $halfMixBoxId")

    val currentTime = now
    val explorer = new BlockExplorer()

    val halfMixBoxConfirmations = explorer.getConfirmationsForBoxId(halfMixBoxId)
    if (halfMixBoxConfirmations >= minConfirmations) {
      ErgoMixCLIUtil.getSpendingTxId(halfMixBoxId).map{fullMixTxId =>
        explorer.getTransaction(fullMixTxId).map{ tx =>
          println(s" Trying to process full mix TxId: $fullMixTxId")

          val boxIds:Seq[String] = tx.outboxes.flatMap(_.getFBox.map(_.id)) // filter(_.mixBox.isDefined).filter() // outputs.filter(_.address == fullMixAddress).map(_.boxId)

          val boxes: Seq[InputBox] = boxIds.flatMap{ boxId => try ctx.getBoxesById(boxId).toList catch {case a:Throwable => Nil}}
          println(s" Found ${boxes.size} full mix boxes")
          val x = new Wallet(masterSecret).getSecret(currentRound).bigInteger
          val gX = ErgoMix.g.exp(x)
          boxes.map{box =>
            val fullMixBox = FullMixBox(box)
            require(fullMixBox.r6 == gX)
            if (fullMixBox.r5 == fullMixBox.r4.exp(x)) { // this is our box
              halfMixTable.update(isSpentCol <-- true).where(mixIdCol === mixId and roundCol === currentRound)
              fullMixTable.insert(mixId, currentRound, currentTime, halfMixBoxId, fullMixBox.id)
              fullMixArchiveTable.insert(mixId, currentRound, currentTime, halfMixBoxId, fullMixBox.id)
              println(s"Successfully processed Half-Mix queue for mixId: $mixId, currentRound: $currentRound, halfMixBoxId: $halfMixBoxId")
            }
          }
        }
      }
    } else {
      println(s"  Insufficient confirmations $halfMixBoxConfirmations for (half mix) mixId: $mixId, halfMixBoxId: $halfMixBoxId, currentRound: $currentRound")
      if (halfMixBoxConfirmations == 0) { // 0 confirmations for fullMixBoxId
        println(s"   Zero conf for (full mix) mixId: $mixId, isAlice: halfMixBoxId: $halfMixBoxId, currentRound: $currentRound")
        optEmissionBoxId.map{emissionBoxId =>
          if (isDoubleSpent(emissionBoxId, explorer, halfMixBoxId)) {
            println(s"    Zero conf for (full mix) mixId: $mixId, halfMixBoxId: $halfMixBoxId, currentRound: $currentRound while emissionBoxId $emissionBoxId spent")
            // the emissionBox used in the fullMix has been spent, while the fullMixBox generated has zero confirmations.
            try {
              undoBadAlice(mixId, currentRound)
              println(s"     Undo success Alice for $mixId, currentRound: $currentRound because halfMixBoxId $halfMixBoxId not spent while emissionBoxId $emissionBoxId spent")
            } catch {
              case a:Throwable =>
                println(s"     Undo ERROR Alice for $mixId, currentRound: $currentRound because halfMixBoxId $halfMixBoxId not spent while emissionBoxId $emissionBoxId spent")
            }
          }
        }
      }
    }
  }

  def processHalfMixQueue:Unit = {
    // Read (from db) our half mix boxes with unspent status
    // Check (from block explorer) if any of those are spent, obtain our full mix box for each spent half mix box, save to full mix table and mark half mix box as spent
    println(s" Processing following half-mixes")

    val halfMixes = halfMixTable.select(
      mixIdCol of halfMixTable,
      roundCol of mixStateTable,
      halfMixBoxIdCol of halfMixTable,
      masterSecretCol of mixRequestsTable
    ).where(
      (isSpentCol of halfMixTable) === false,
      (mixIdCol of halfMixTable) === (mixIdCol of mixRequestsTable),
      (mixIdCol of halfMixTable) === (mixIdCol of mixStateTable),
      (roundCol of halfMixTable) === (roundCol of mixStateTable)
    ).as{case ar =>
      val i = ar.toIterator
      (
        i.next.as[String], // mix ID
        i.next.as[Int], // current round
        i.next.as[String], // half mix box id
        i.next.as[BigDecimal].toBigInt() // master secret
      )
    }

    halfMixes foreach (x => println(s"  > ${x._1}"))

    halfMixes.map{
      case (mixId, currentRound, halfMixBoxId, masterSecret) =>
        try {
          val optEmissionBoxId = spentFeeEmissionBoxTable.select(boxIdCol).where(mixIdCol === mixId, roundCol === currentRound).firstAsT[String].headOption
          processHalfMix(mixId, currentRound, halfMixBoxId, masterSecret, optEmissionBoxId)
        } catch {
          case a:Throwable =>
            println(s" [HalfMix ${mixId}] ERROR in currentRound: $currentRound: ${a.getMessage}")
        }
    }
  }

  def processDeposits:Unit = {
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

  def insertDeposit(address: String, amount:Long, boxId:String) = {
    // does not check from block explorer. Should be used by experts only. Otherwise, the job will automatically insert deposits
    if (mixRequestsTable.exists(depositAddressCol === address)) {
      if (unspentDepositsTable.exists(boxIdCol === boxId)) throw new Exception(s"Deposit already exists")
      if (spentDepositsTable.exists(boxIdCol === boxId)) throw new Exception(s"Deposit already spent")
      unspentDepositsTable.insert(address, boxId, amount, now)
      unspentDepositsArchiveTable.insert(address, boxId, amount, now)
      val currentSum = unspentDepositsTable.select(amountCol).where(addressCol === address).firstAsT[Long].sum
      val needed = ErgoMix.mixAmount + ErgoMix.feeAmount
      if (currentSum >= needed) {
        mixRequestsTable.update(depositCompletedCol <-- true).where(depositAddressCol === address)
        "deposit completed"
      } else s"${needed - currentSum} nanoErgs pending"
    } else throw new Exception(s"Address $address does not belong to this wallet")
  }

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
   Note that 5 is already implemented as undoWithdraw below. TODO: Implement undo for 1

   The case for 2, 3, 4 is implemented below. Since these transactions can be undone due to a double spend, they must be handed automatically via jobs

   TODO: Track spent boxes so that above undo operations (1 and 5) are done via a job in case of a fork
  */

  def undoBadBob(mixId:String, round:Int) = {
    val $INFO$ = """Warning! This can cause mix corruption with wrong inputs"""
    undoMixState(mixId, round)
    fullMixTable.deleteWhere(mixIdCol === mixId, roundCol === round)
    if (round == 0) {
      val address = mixRequestsTable.select(depositAddressCol).where(mixIdCol === mixId).firstAsT[String].headOption.map{depositAddress =>
        undoSpends(depositAddress)
      }
    }
  }

  def undoBadAlice(mixId:String, round:Int) = {
    val $INFO$ = """Warning! This can cause mix corruption with wrong inputs"""
    undoMixState(mixId, round)
    halfMixTable.deleteWhere(mixIdCol === mixId, roundCol === round)
    if (round == 0) {
      val address = mixRequestsTable.select(depositAddressCol).where(mixIdCol === mixId).firstAsT[String].headOption.map{depositAddress =>
        undoSpends(depositAddress)
      }
    }
  }

  def undoWithdraw(txId:String) = ErgoMixCLIUtil.usingClient{implicit ctx =>
    // should be used only in case of fork. Leaving it to be called manually for now.
    val explorer = new BlockExplorer()
    if (explorer.getTransaction(txId).isDefined) throw new Exception("Transaction already confirmed")

    mixRequestsTable.select(mixIdCol of withdrawTable).where(
      (txIdCol of withdrawTable) === txId,
      (mixIdCol of mixRequestsTable) === (mixIdCol of withdrawTable),
      (mixStatusCol of mixRequestsTable) === Complete.value
    ).firstAsT[String].headOption.fold(
      throw new Exception("No withdraw found")
    ){mixId =>
      withdrawTable.deleteWhere(txIdCol === txId)
      mixRequestsTable.update(mixStatusCol <-- Running.value).where(mixIdCol === mixId)
    }
  }

  def rescanAllMixes = {
    mixRequestsTable.select(mixIdCol).where(mixStatusCol === Running.value).firstAsT[String].map(rescanMix)
  }

  def rescanMix(mixId:String):Unit = ErgoMixCLIUtil.usingClient{implicit ctx =>
    val explorer = new BlockExplorer
    val fullMixes = fullMixTable.select(fullMixBoxIdCol, roundCol, masterSecretCol of mixRequestsTable).where(
      mixIdCol === mixId,
      mixIdCol === (mixIdCol of mixStateTable),
      roundCol === (roundCol of mixStateTable),
      mixIdCol === (mixIdCol of mixRequestsTable)
    ).as{a =>
      (a(0).as[String], a(1).as[Int], a(2).as[BigDecimal].toBigInt())
    }
    println(s"[MixID $mixId] Found ${fullMixes.size} pending rows for fullMix")
    assert(fullMixes.size <= 1)

    val optFullMix = fullMixes.headOption

    if (optFullMix.isDefined) {
      optFullMix.map{
        case (fullMixBoxId, currentRound, masterSecret) =>
          println(s" [MixID $mixId] Processing fullMixBoxId $fullMixBoxId of round: $currentRound")
          val nextRound = currentRound+1
          val secret = new Wallet(masterSecret).getSecret(nextRound)
          val gZ = ErgoMix.g.exp(secret.bigInteger)
          explorer.getSpendingTxId(fullMixBoxId).map {txId =>
            println(s"  [MixID $mixId] fullMixBoxId $fullMixBoxId of round $currentRound spent with txId: $txId")
            explorer.getTransaction(txId).map{ tx =>
              println(s"   [MixID $mixId] Found ${tx.outboxes.size} output boxes when scanning fullMixBoxId: $fullMixBoxId, round: $currentRound, txId: $txId")
              tx.outboxes.map{outbox =>
                outbox.mixBox match {
                  case Some(Left(HBox(boxId, `gZ`))) => // spent as Alice, half mix box at output #01
                    println(s"    [MixID $mixId] Found half mix box $boxId remixed from fullMixBoxId: $fullMixBoxId, round: $currentRound, txId: $txId")
                    halfMixTable.insert(mixId, nextRound, now, boxId, false)
                    mixStateTable.update(isAliceCol <-- true, roundCol <-- nextRound).where(mixIdCol === mixId)
                    println(s"     [MixID $mixId] Updated next round status for $boxId remixed from fullMixBoxId: $fullMixBoxId, round: $currentRound, txId: $txId")
                    rescanMix(mixId)
                  case Some(Right(FBox(boxId, g4, `gZ`, g6))) => // spent as Bob, full mix boxes at outputs #0, #1
                    println(s"    [MixID $mixId] Found full mix box $boxId remixed from fullMixBoxId: $fullMixBoxId, round: $currentRound, txId: $txId")
                    val halfMixBoxes = tx.inboxes.filter(_.isHalfMixBox)
                    assert(halfMixBoxes.size == 1)
                    val halfMixBoxId = halfMixBoxes.head.id
                    println(s"     [MixID $mixId] Found half mix box $halfMixBoxId as input for full mix tx $txId")
                    fullMixTable.insert(mixId, nextRound, now, halfMixBoxId, boxId)
                    mixStateTable.update(isAliceCol <-- false, roundCol <-- nextRound).where(mixIdCol === mixId)
                    println(s"      [MixID $mixId] Updated next round status for $boxId remixed from halfMixBodIx: $halfMixBoxId, fullMixBoxId: $fullMixBoxId, round: $currentRound, txId: $txId")
                    rescanMix(mixId)
                  case any =>
                    println(s"    [MixID $mixId] Found unknown: $any (w.r.t. boxId: ${outbox.id}) when scanning remix of fullMixBoxId: $fullMixBoxId, round: $currentRound, txId: $txId")
                }
              }
            }
          }
      }
    } else {
      val halfMixes = halfMixTable.select(halfMixBoxIdCol, roundCol, masterSecretCol of mixRequestsTable).where(
        mixIdCol === mixId,
        isAliceCol.of(mixStateTable) === true,
        isSpentCol === false,
        mixIdCol === (mixIdCol of mixStateTable),
        roundCol === (roundCol of mixStateTable),
        mixIdCol === (mixIdCol of mixRequestsTable)
      ).as{a =>
        (a(0).as[String], a(1).as[Int], a(2).as[BigDecimal].toBigInt())
      }
      println(s"[MixID $mixId] Found ${halfMixes.size} pending rows for halfMix")
      assert(halfMixes.size <= 1)
      halfMixes.headOption.map{
        case (halfMixBoxId, currentRound, masterSecret) =>
          println(s" [MixID $mixId] Processing halfMixBoxId $halfMixBoxId of round: $currentRound")
          val secret = new Wallet(masterSecret).getSecret(currentRound).bigInteger
          val gZ = ErgoMix.g.exp(secret)

          explorer.getSpendingTxId(halfMixBoxId).map{ txId =>
            println(s"  [MixID $mixId] halfMixBoxId $txId of round $currentRound spent with txId: $txId")
            explorer.getTransaction(txId).map{ tx =>
              println(s"   [MixID $mixId] Found ${tx.outboxes.size} output boxes when scanning halfMixBoxId: $txId, round: $currentRound, txId: $txId")
              tx.outboxes.map{outbox =>
                outbox.mixBox match {
                  case Some(Right(FBox(boxId, r4, r5, `gZ`))) if r4.exp(secret) == r5 =>
                    println(s"    [MixID $mixId] Found full mix box $boxId mixed from halfMixBoxId: $halfMixBoxId, round: $currentRound, txId: $txId")
                    fullMixTable.insert(mixId, currentRound, now, halfMixBoxId, boxId)
                    halfMixTable.update(isSpentCol <-- true).where(mixIdCol === mixId, roundCol === currentRound)
                    rescanMix(mixId)
                  case any => // do nothing
                    println(s"    [MixID $mixId] Found unknown: $any when scanning remix of halfMixBoxId: $halfMixBoxId, round: $currentRound, txId: $txId")
                }
              }
            }
          }
      }
    }
  }
}
