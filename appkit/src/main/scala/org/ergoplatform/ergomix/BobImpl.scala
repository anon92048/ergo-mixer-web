package org.ergoplatform.ergomix

import java.math.BigInteger

import org.ergoplatform.appkit._
import org.ergoplatform.appkit.impl.ErgoTreeContract
import sigmastate.eval._
import special.sigma.GroupElement

import scala.jdk.CollectionConverters._
import ErgoMix._

class BobImpl(y:BigInteger)(implicit ctx: BlockchainContext) extends Bob {
  val gY: GroupElement = g.exp(y)

  implicit val ergoMix = new ErgoMix(ctx)

  val util = new Util()

  def spendFullMixBox(f: FullMixBox, endBoxes: Seq[EndBox], feeAmount:Long, otherInputBoxes:Array[InputBox], changeAddress:String, changeBoxRegs:Seq[ErgoValue[_]], additionalDlogSecrets:Array[BigInteger], additionalDHTuples:Array[DHT]): SignedTransaction = {
    val txB = ctx.newTxBuilder

    val outBoxes: Seq[OutBox] = endBoxes.map{ endBox =>
      val outBoxBuilder = txB.outBoxBuilder().value(endBox.value).contract(new ErgoTreeContract(endBox.receiverBoxScript))
      (if (endBox.receiverBoxRegs.isEmpty) outBoxBuilder else outBoxBuilder.registers(endBox.receiverBoxRegs:_*)).build()
    }

    val inputs = new java.util.ArrayList[InputBox]()

    otherInputBoxes.foreach(inputs.add)
    inputs.add(f.inputBox) // add the fullmix box as the last, that way, we can have an halfmix box as the first input

    val txToSign = txB.boxesToSpend(inputs)
      .outputs(outBoxes: _*)
      .fee(feeAmount)
      .sendChangeTo(util.getAddress(changeAddress), changeBoxRegs:_*)
      .build()

    val bob: ErgoProver = additionalDHTuples.foldLeft(
      additionalDlogSecrets.foldLeft(
        ctx.newProverBuilder().withDLogSecret(y)
      )(
        (ergoProverBuilder, bigInteger) => ergoProverBuilder.withDLogSecret(bigInteger)
      )
    )(
      (ergoProverBuilder, dh) => ergoProverBuilder.withDHTData(dh.gv, dh.hv, dh.uv, dh.vv, dh.x)
    ).build()

    bob.sign(txToSign)
  }

  def spendHalfMixBox(halfMixBox: HalfMixBox, inputBoxes:Array[InputBox], fee:Long, changeAddress:String, additionalDlogSecrets:Array[BigInteger], additionalDHTuples:Array[DHT]): (FullMixTx, Boolean) = {
    val gXY: GroupElement = halfMixBox.gX.exp(y)
    val bit: Boolean = scala.util.Random.nextBoolean()

    val (c1, c2) = if (bit) (gY, gXY) else (gXY, gY)
    val txB = ctx.newTxBuilder()

    val firstOutBox = txB.outBoxBuilder().value(
      halfMixBox.inputBox.getValue
    ).registers(
      ErgoValue.of(c1), ErgoValue.of(c2), ErgoValue.of(halfMixBox.gX)
    ).contract(
      ergoMix.fullMixScriptContract
    ).build()

    val secondOutBox = txB.outBoxBuilder().value(
      halfMixBox.inputBox.getValue
    ).registers(
      ErgoValue.of(c2), ErgoValue.of(c1), ErgoValue.of(halfMixBox.gX)
    ).contract(
      ergoMix.fullMixScriptContract
    ).build()

    val inputs = new java.util.ArrayList[InputBox]()

    inputs.add(halfMixBox.inputBox)
    inputs.addAll(inputBoxes.toList.asJava)

    val txToSign = txB.boxesToSpend(inputs)
      .outputs(firstOutBox, secondOutBox)
      .fee(fee)
      .sendChangeTo(util.getAddress(changeAddress))
      .build()

    val bob: ErgoProver = additionalDHTuples.foldLeft(
      additionalDlogSecrets.foldLeft(
        ctx.newProverBuilder().withDHTData(g, halfMixBox.gX, gY, gXY, y)
      )(
        (ergoProverBuilder, bigInteger) => ergoProverBuilder.withDLogSecret(bigInteger)
      )
    )(
      (ergoProverBuilder, dh) => ergoProverBuilder.withDHTData(dh.gv, dh.hv, dh.uv, dh.vv, dh.x)
    ).build()

    (FullMixTx(bob.sign(txToSign)), bit)
  }
}
