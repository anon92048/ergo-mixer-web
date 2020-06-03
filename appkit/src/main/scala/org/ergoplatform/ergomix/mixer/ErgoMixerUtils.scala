package org.ergoplatform.ergomix.mixer

import java.security.SecureRandom

import org.ergoplatform.appkit.{ConstantsBuilder, ErgoContract, ErgoToken, ErgoValue, InputBox}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.ergomix
import org.ergoplatform.ergomix.ErgoMix.g
import org.ergoplatform.ergomix.cli.ErgoMixCLIUtil.usingClient
import org.ergoplatform.ergomix.cli.{Carol, ErgoMixCLIUtil}
import org.ergoplatform.ergomix.mixer.Models.{FBox, HBox, OutBox}
import org.ergoplatform.ergomix.{ErgoMix, Token, Util => EUtil}
import special.sigma.GroupElement
import sigmastate.eval._

object ErgoMixerUtil {

  // The minimum number of confirmations for a current mix transaction before proceeding to next step
  val minConfirmations = 3

  def isDoubleSpent(boxId:String, wrtBoxId:String): Boolean = ErgoMixCLIUtil.usingClient{ implicit ctx =>
    val explorer: BlockExplorer = new BlockExplorer()
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

  def generateToken(inputBoxIds:Array[String], dlogSecret:BigInt, outputAddress:String, tokenAmount:Long, amount:Long) = {
    ErgoMixCLIUtil.usingClient{implicit ctx =>
      val inputBoxes = ctx.getBoxesById(inputBoxIds: _*)
      val txB = ctx.newTxBuilder()
      val util = new EUtil()
      val tokenId: String = inputBoxes(0).getId.toString
      val outBox = txB.outBoxBuilder().value(amount).tokens(new ErgoToken(tokenId, tokenAmount)).contract(new ErgoTreeContract(util.getAddress(outputAddress).script)).build()

      val inputs = new java.util.ArrayList[InputBox]()
      inputBoxes.foreach(inputs.add)
      val txToSign = txB.boxesToSpend(inputs)
        .outputs(outBox)
        .fee(ErgoMix.feeAmount)
        .sendChangeTo(util.getAddress(Carol.getProveDlogAddress(dlogSecret)))
        .build()

      val sender = ctx.newProverBuilder().withDLogSecret(dlogSecret.bigInteger).build()
      val tx = sender.sign(txToSign)
      ctx.sendTransaction(tx)
      tx.toJson(false)
    }
  }

  val tokenEmissionBoxScript =
    """{
      |  val gZ = SELF.R4[GroupElement].get
      |  val isCopy = {(b:Box) =>
      |     b.propositionBytes == SELF.propositionBytes && b.value == SELF.value &&
      |     b.tokens(0)._1 == SELF.tokens(0)._1 &&
      |     b.tokens(0)._2 == (SELF.tokens(0)._2 - 1) &&
      |     b.R4[GroupElement].get == gZ
      |  }
      |  proveDlog(gZ) || sigmaProp(OUTPUTS.exists(isCopy))
      |}
      |""".stripMargin

  def getTokenEmissionContract = {
    ErgoMixCLIUtil.usingClient { implicit ctx =>
      val util = new EUtil()

      val tokenBoxContract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.empty(),
        tokenEmissionBoxScript
      )
      val address = util.getAddressFromProposition(tokenBoxContract.getErgoTree).toString
      val contract = tokenBoxContract.getErgoScript
      Array(address, contract)
    }
  }

  val startNumTokens = 100000000000000L

  def spendTokenEmissionBox(inputBoxIds:Array[String], dlogSecret:BigInt, tokenEmissionBoxId: String, outputAddress:String, changeAddress:String) = {
    ErgoMixCLIUtil.usingClient { implicit ctx =>
      val txB = ctx.newTxBuilder()

      val tokenBoxContract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.empty(),
        tokenEmissionBoxScript
      )

      val tokenBox: OutBox = new BlockExplorer().getBoxById(tokenEmissionBoxId)
      val token: Token = tokenBox.tokens(0)
      val r4: GroupElement = tokenBox.ge("R4")

      val newTokenBox = txB.outBoxBuilder().value(tokenBox.amount).tokens(new ErgoToken(token.id, token.value - 1)).registers(ErgoValue.of(r4)).contract(tokenBoxContract).build()
      val util = new EUtil()

      val myBox = txB.outBoxBuilder().value(ErgoMix.mixAmount+ErgoMix.feeAmount).tokens(new ErgoToken(token.id, 1)).contract(new ErgoTreeContract(util.getAddress(outputAddress).script)).build()

      val inputs = new java.util.ArrayList[InputBox]()
      val inputBoxes = ctx.getBoxesById(inputBoxIds: _*)
      inputBoxes.foreach(inputs.add)

      val tokenInputBox = ctx.getBoxesById(tokenEmissionBoxId)(0)
      inputs.add(tokenInputBox)

      val txToSign = txB.boxesToSpend(inputs)
        .outputs(myBox, newTokenBox)
        .fee(ErgoMix.feeAmount)
        .sendChangeTo(util.getAddress(changeAddress))
        .build()

      val sender = ctx.newProverBuilder().withDLogSecret(dlogSecret.bigInteger).build()
      val tx = sender.sign(txToSign)
      ctx.sendTransaction(tx)
      tx.toJson(false)

    }
  }

  val minErgsInBox = 1500000L
  def generateTokenEmissionBox(inputBoxIds:Array[String], dlogSecret:BigInt, changeAddress:String, z:BigInt) = {

    ErgoMixCLIUtil.usingClient{implicit ctx =>

      val inputBoxes = ctx.getBoxesById(inputBoxIds: _*)
      val tokenId: String = inputBoxes(0).getId.toString

      val tokenBoxContract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.empty(),
        tokenEmissionBoxScript
      )

      val gZ:GroupElement = g.exp(z.bigInteger)
      val txB = ctx.newTxBuilder()

      val outBox = txB.outBoxBuilder().value(minErgsInBox).tokens(new ErgoToken(tokenId, startNumTokens)).registers(ErgoValue.of(gZ)).contract(tokenBoxContract).build()

      val inputs = new java.util.ArrayList[InputBox]()
      inputBoxes.foreach(inputs.add)

      val util = new EUtil()

      val txToSign = txB.boxesToSpend(inputs)
        .outputs(outBox)
        .fee(ErgoMix.feeAmount)
        .sendChangeTo(util.getAddress(changeAddress))
        .build()

      val sender = ctx.newProverBuilder().withDLogSecret(dlogSecret.bigInteger).build()
      val tx = sender.sign(txToSign)
      ctx.sendTransaction(tx)
      tx.toJson(false)
    }
  }

  def getFeeEmissionBoxes = ErgoMixCLIUtil.getFeeEmissionBoxes

  def getHalfMixBoxes = ErgoMixCLIUtil.getHalfMixBoxes

  def generateProveDlogAddress = {
    val secret = Util.randBigInt
    val address = Carol.getProveDlogAddress(secret)
    Array(
      "The following rows contain the secret and the withdraw address. Please save the secret as it is not stored",
      secret.toString,
      address
    )

  }

  def createFeeEmissionBox(inputBoxIds:Array[String], dlogSecret:BigInt, amount:Long, feeEmissionBoxSecret:BigInt, changeAddress:String) = {
    ErgoMixCLIUtil.usingClient { implicit ctx =>
      val arr = Carol.createFeeEmissionBox(feeEmissionBoxSecret, amount, inputBoxIds, changeAddress, dlogSecret.toString())
      arr(1)
    }
  }

  def send(inputBoxIds:Array[String], dlogSecret:BigInt, outputAddress:String, amount:Long) = {
    ErgoMixCLIUtil.usingClient{implicit ctx =>
      val inputBoxes = ctx.getBoxesById(inputBoxIds: _*)
      val txB = ctx.newTxBuilder()
      val util = new EUtil()
      val outBox = txB.outBoxBuilder().value(amount).contract(new ErgoTreeContract(util.getAddress(outputAddress).script)).build()

      val inputs = new java.util.ArrayList[InputBox]()
      inputBoxes.foreach(inputs.add)
      val txToSign = txB.boxesToSpend(inputs)
        .outputs(outBox)
        .fee(ErgoMix.feeAmount)
        .sendChangeTo(util.getAddress(Carol.getProveDlogAddress(dlogSecret)))
        .build()

      val sender = ctx.newProverBuilder().withDLogSecret(dlogSecret.bigInteger).build()
      val tx = sender.sign(txToSign)
      ctx.sendTransaction(tx)
      tx.toJson(false)
    }
  }

  def getBoxById(boxId:String) = usingClient{implicit ctx =>
    val explorer = new BlockExplorer()
    explorer.getBoxById(boxId)
  }

  def getMixBox(boxId:String): Option[Either[HBox, FBox]] = getBoxById(boxId).mixBox

  /* General util methods, for testing or debugging

  def getUnspentBoxWithId(boxId:String) = {
    ErgoMixCLIUtil.usingClient{implicit ctx =>
      ctx.getBoxesById(boxId).headOption.map(box => new EUtil().getAddressFromProposition(box.getErgoTree).toString)
    }
  }

  def getConfirmationsForBoxId(boxId:String) = {
    ErgoMixCLIUtil.getConfirmationsForBoxId(boxId)
  }

  def getSpendingTxId(boxId:String) = {
    ErgoMixCLIUtil.getSpendingTxId(boxId)
  }

  def getTransactionOutputs(txId:String) = {
    ErgoMixCLIUtil.getTransactionOutputs(txId).getOrElse(Nil)
  }

  def getBoxById(boxId:String) = {
    ErgoMixCLIUtil.getBoxById(boxId).toJson(false)
  }
   */

}

