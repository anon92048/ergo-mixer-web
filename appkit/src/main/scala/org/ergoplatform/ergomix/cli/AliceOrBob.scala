package org.ergoplatform.ergomix.cli


import java.math.BigInteger

import org.ergoplatform.appkit.{ErgoValue, InputBox, SignedTransaction}
import org.ergoplatform.ergomix.ErgoMix._
import org.ergoplatform.ergomix.cli.ErgoMixCLIUtil.{getProver, usingClient}
import org.ergoplatform.ergomix._
import special.sigma.GroupElement

object AliceOrBob {
  /*
Play Alice's or Bob's role in spending a full-mix box with secret.
fullMixBoxId is the boxId of the full-mix box to spend.
withdrawAddress is the address where the funds are to be sent.

The method attempts to create a transaction outputting a half-mix box at index 0.
   */
  def spendFullMixBox(isAlice:Boolean, secret:BigInt,
                      fullMixBoxId:String, withdrawAddress:String):String = {
    usingClient{implicit ctx =>
      val tx = spendFullMixBox(isAlice, secret, fullMixBoxId, withdrawAddress, Array[String](), ErgoMix.feeAmount, withdrawAddress, true)
      tx.toJson(false)
    }
  }

  /*
Play Alice's or Bob's role in spending a full-mix box with secret.
fullMixBoxId is the boxId of the full-mix box to spend.
withdrawAddress is the address where the funds are to be sent.
inputBoxIds are boxIds of input boxes funding the transaction.
Signing may require several secrets for proveDLog which are supplied in the array proveDlogSecrets.
Signing may also require several tuples of type (g, h, u, v, x) for proveDHTuple.
The arrays proverDHT_g, proverDHT_h, proverDHT_u, proverDHT_v, proverDHT_x must have equal number of elements, one for each such tuple.

The method attempts to create a transaction outputting a half-mix box at index 0.
If broadCast is false it just outputs the transaction but does not broadcast it.

feeAmount is the amount in fee in nanoErgs
   */
  def spendFullMixBox(isAlice: Boolean, secret: BigInt, fullMixBoxId: String, withdrawAddress: String, inputBoxIds: Array[String], feeAmount: Long, changeAddress: String, broadCast: Boolean) = {
    usingClient{implicit ctx =>
      val alice_or_bob = getProver(secret, isAlice)
      val fullMixBox: InputBox = ctx.getBoxesById(fullMixBoxId)(0)
      val endBox = EndBox(new Util().getAddress(withdrawAddress).script, Nil, if (inputBoxIds.nonEmpty) fullMixBox.getValue else fullMixBox.getValue - feeAmount)
      val tx: SignedTransaction = alice_or_bob.spendFullMixBox(FullMixBox(fullMixBox), Seq(endBox), feeAmount, inputBoxIds, changeAddress, Nil, Array[BigInteger](), Array[DHT]())
      if (broadCast) ctx.sendTransaction(tx)
      tx
    }
  }

  /*
Play Alice's or Bob's role in spending a full-mix box with secret to generate a new half-mix box for remixing.
That is, perform the first step of the next round by behaving like Alice (by creating a new half-mix box)

fullMixBoxId is the boxId of the full-mix box to spend.
feeEmissionBoxId is the boxId of input boxes funding the transaction
The method attempts to create a transaction outputting a half-mix box at index 0.
   */
  def spendFullMixBox_RemixAsAlice(isAlice:Boolean, secret:BigInt,
                                   fullMixBoxId:String, nextSecret:BigInt, feeEmissionBoxId:String): HalfMixTx = {
    usingClient{implicit ctx =>
      val feeEmissionBox = ctx.getBoxesById(feeEmissionBoxId)(0)
      val feeEmissionBoxAddress = new Util().addressEncoder.fromProposition(feeEmissionBox.getErgoTree).get.toString
      val gZ = ErgoValue.of(feeEmissionBox.getRegisters.get(0).getValue.asInstanceOf[GroupElement])
      spendFullMixBox_RemixAsAlice(isAlice, secret, fullMixBoxId, nextSecret, Array(feeEmissionBoxId), ErgoMix.feeAmount, feeEmissionBoxAddress, Seq(gZ), true)
    }
  }

  /*
Play Alice's or Bob's role in spending a full-mix box with secret to generate a new half-mix box for remixing.
That is, perform the first step of the next round by behaving like Alice (by creating a new half-mix box)

fullMixBoxId is the boxId of the full-mix box to spend.
inputBoxIds are boxIds of input boxes funding the transaction.
Signing may require several secrets for proveDLog which are supplied in the array proveDlogSecrets.
Signing may also require several tuples of type (g, h, u, v, x) for proveDHTuple.
The arrays proverDHT_g, proverDHT_h, proverDHT_u, proverDHT_v, proverDHT_x must have equal number of elements, one for each such tuple.

The method attempts to create a transaction outputting a half-mix box at index 0.
If broadCast is false it just outputs the transaction but does not broadcast it.

feeAmount is the amount in fee in nanoErgs
   */

  private def spendFullMixBox_RemixAsAlice(isAlice: Boolean, secret: BigInt, fullMixBoxId: String, nextSecret: BigInt, inputBoxIds: Array[String], feeAmount: Long, changeAddress: String, changeBoxRegs:Seq[ErgoValue[_]], broadCast: Boolean): HalfMixTx = {
    usingClient{implicit ctx =>
      val alice_or_bob = getProver(secret, isAlice)
      val fullMixBox: InputBox = ctx.getBoxesById(fullMixBoxId)(0)
      val halfMixTx = alice_or_bob.spendFullMixBoxNextAlice(FullMixBox(fullMixBox), nextSecret.bigInteger, feeAmount, inputBoxIds, changeAddress, changeBoxRegs, Array[BigInteger](), Array[DHT]())
      if (broadCast) ctx.sendTransaction(halfMixTx.tx)
      halfMixTx
    }
  }
  /*
Play Alice's or Bob's role in spending a full-mix box with secret in another full-mix transaction that spends some half-mix box with this full-mix box.
That is, perform the first step of the next round by behaving like Bob (by creating a two new full-mix boxes)

fullMixBoxId is the boxId of the full-mix box to spend.
feeEmissionBoxId is the boxId of input boxes funding the transaction
The method attempts to create a transaction outputting a half-mix box at index 0.
   */
  def spendFullMixBox_RemixAsBob(isAlice:Boolean, secret:BigInt,
                                 fullMixBoxId:String, nextSecret:BigInt, nextHalfMixBoxId:String, feeEmissionBoxId:String): (FullMixTx, Boolean) = {
    usingClient{implicit ctx =>
      val feeEmissionBox = ctx.getBoxesById(feeEmissionBoxId)(0)
      val feeEmissionBoxAddress = new Util().addressEncoder.fromProposition(feeEmissionBox.getErgoTree).get.toString
      val gZ = ErgoValue.of(feeEmissionBox.getRegisters.get(0).getValue.asInstanceOf[GroupElement])
      spendFullMixBox_RemixAsBob(isAlice, secret, fullMixBoxId, nextSecret, nextHalfMixBoxId, Array(feeEmissionBoxId), feeAmount, feeEmissionBoxAddress, Seq(gZ), true)
    }
  }

  /*
Play Alice's or Bob's role in spending a full-mix box with secret in another full-mix transaction that spends some half-mix box with this full-mix box.
That is, perform the first step of the next round by behaving like Bob (by creating a two new full-mix boxes)

fullMixBoxId is the boxId of the full-mix box to spend.
inputBoxIds are boxIds of input boxes funding the transaction.
Signing may require several secrets for proveDLog which are supplied in the array proveDlogSecrets.
Signing may also require several tuples of type (g, h, u, v, x) for proveDHTuple.
The arrays proverDHT_g, proverDHT_h, proverDHT_u, proverDHT_v, proverDHT_x must have equal number of elements, one for each such tuple.

The method attempts to create a transaction outputting a half-mix box at index 0.
If broadCast is false it just outputs the transaction but does not broadcast it.

feeAmount is the amount in fee in nanoErgs
   */
  private def spendFullMixBox_RemixAsBob(isAlice: Boolean, secret: BigInt, fullMixBoxId: String, nextSecret: BigInt, nextHalfMixBoxId: String, inputBoxIds: Array[String], feeAmount: Long, changeAddress: String, changeBoxRegs:Seq[ErgoValue[_]], broadCast: Boolean): (FullMixTx, Boolean) = {
    usingClient{implicit ctx =>
      val alice_or_bob:FullMixBoxSpender = if (isAlice) new AliceImpl(secret.bigInteger) else new BobImpl(secret.bigInteger)
      val fullMixBox: InputBox = ctx.getBoxesById(fullMixBoxId)(0)
      val halfMixBox = ctx.getBoxesById(nextHalfMixBoxId)(0)
      val (fullMixTx, bit) = alice_or_bob.spendFullMixBoxNextBob(FullMixBox(fullMixBox), HalfMixBox(halfMixBox), nextSecret.bigInteger, feeAmount, inputBoxIds, changeAddress, changeBoxRegs, Array[BigInteger](), Array[DHT]())
      if (broadCast) ctx.sendTransaction(fullMixTx.tx)
      (fullMixTx, bit)
    }
  }
}
