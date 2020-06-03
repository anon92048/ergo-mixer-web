package org.ergoplatform.ergomix

import java.math.BigInteger

import org.ergoplatform.appkit._
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.ergomix.ErgoMix._
import sigmastate.eval._
import special.sigma.GroupElement

import scala.jdk.CollectionConverters._

class AliceImpl (x:BigInteger) (implicit ctx: BlockchainContext) extends Alice {

  val gX: GroupElement = g.exp(x)

  implicit  val ergoMix = new ErgoMix(ctx)
  val util = new Util()
  def spendFullMixBox(fullMixBox: FullMixBox, endBoxes: Seq[EndBox], feeAmount:Long, otherInputBoxes:Array[InputBox], changeAddress:String, changeBoxRegs:Seq[ErgoValue[_]], additionalDlogSecrets:Array[BigInteger], additionalDHTuples:Array[DHT]): SignedTransaction = {

    val (gY, gXY) = (fullMixBox.r4, fullMixBox.r5)
    val txB: UnsignedTransactionBuilder = ctx.newTxBuilder

    val outBoxes: Seq[OutBox] = endBoxes.map{ endBox =>
      val outBoxBuilder = outBoxBuilderWithTokens(txB.outBoxBuilder().value(endBox.value).contract(new ErgoTreeContract(endBox.receiverBoxScript)))(endBox.tokens)

      (if (endBox.receiverBoxRegs.isEmpty) outBoxBuilder else outBoxBuilder.registers(endBox.receiverBoxRegs:_*)).build()
    }

    val inputs = new java.util.ArrayList[InputBox]()

    otherInputBoxes.foreach(inputs.add)

    inputs.add(fullMixBox.inputBox) // add the fullMixBox as the last, that way, we can have an HalfMixBox as the first input

    val txToSign = ctx.newTxBuilder.boxesToSpend(inputs)
      .outputs(outBoxes:_*)
      .fee(feeAmount)
      .sendChangeTo(util.getAddress(changeAddress), changeBoxRegs:_*)
      .build()

    val alice: ErgoProver = additionalDHTuples.foldLeft(
      additionalDlogSecrets.foldLeft(
        ctx.newProverBuilder().withDHTData(g, gY, gX, gXY, x)
      )(
        (ergoProverBuilder, bigInteger) => ergoProverBuilder.withDLogSecret(bigInteger)
      )
    )(
      (ergoProverBuilder, dh) => ergoProverBuilder.withDHTData(dh.gv, dh.hv, dh.uv, dh.vv, dh.x)
    ).build()

    alice.sign(txToSign)
  }

  def createHalfMixBox(inputBoxes:Array[InputBox], feeAmount:Long, changeAddress:String, additionalDlogSecrets:Array[BigInteger], additionalDHTuples:Array[DHT]): HalfMixTx = {
    val txB: UnsignedTransactionBuilder = ctx.newTxBuilder

    val newBox = outBoxBuilderWithTokens(
      txB.outBoxBuilder().value(
        ErgoMix.mixAmount
      ).registers(
        ErgoValue.of(gX)
      ).contract(
        ergoMix.halfMixContract
      )
    )(inputBoxes.uniqueTokens).build()

    val inputs = new java.util.ArrayList[InputBox]()

    inputs.addAll(inputBoxes.toList.asJava)

    val txToSign = txB.boxesToSpend(inputs)
      .outputs(newBox)
      .fee(feeAmount)
      .sendChangeTo(util.getAddress(changeAddress))
      .build()

    val alice: ErgoProver = additionalDHTuples.foldLeft(
      additionalDlogSecrets.foldLeft(
        ctx.newProverBuilder()
      )(
        (ergoProverBuilder, bigInteger) => ergoProverBuilder.withDLogSecret(bigInteger)
      )
    )(
      (ergoProverBuilder, dh) => ergoProverBuilder.withDHTData(dh.gv, dh.hv, dh.uv, dh.vv, dh.x)
    ).build()

    HalfMixTx(alice.sign(txToSign))
  }
}
