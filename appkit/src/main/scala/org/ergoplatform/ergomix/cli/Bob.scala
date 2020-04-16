package org.ergoplatform.ergomix.cli

import java.math.BigInteger

import org.ergoplatform.appkit.InputBox
import org.ergoplatform.ergomix.ErgoMix.feeAmount
import org.ergoplatform.ergomix.cli.ErgoMixCLIUtil.usingClient
import org.ergoplatform.ergomix.{BobImpl, DHT, HalfMixBox}

object Bob {
  /*
Play Bob's role in creating a full-mix box with secret y.
halfMixBoxId is the boxId of the half-mix box created by an instance of Alice.
inputBoxIds are boxIds of additional input boxes funding the transaction.
Signing may require an additional dLog which is supplied as proveDlogSecret.
The method attempts to create a transaction outputting two full-mix box at index 0 and 1
   */
  def spendHalfMixBox(y:BigInt, halfMixBoxId:String, otherInputBoxIds:Array[String], changeAddress:String, otherDlogSecret:String):Array[String] = {
    val (fullMixTx, bit) = spendHalfMixBox(y, halfMixBoxId, otherInputBoxIds, feeAmount, changeAddress, Array(otherDlogSecret), true)
    Array(fullMixTx.tx.toJson(false), bit.toString)
  }

  /*
Play Bob's role in creating a full-mix box with secret y.
halfMixBoxId is the boxId of the half-mix box created by an instance of Alice.
inputBoxIds are boxIds of additional input boxes funding the transaction.
Signing may require several secrets for proveDLog which are supplied in the array proveDlogSecrets.
Signing may also require several tuples of type (g, h, u, v, x) for proveDHTuple.
The arrays proverDHT_g, proverDHT_h, proverDHT_u, proverDHT_v, proverDHT_x must have equal number of elements, one for each such tuple.

The method attempts to create a transaction outputting two full-mix box at index 0 and 1.
If broadCast is false it just outputs the transaction but does not broadcast it.

feeAmount is the amount in fee in nanoErgs
   */

  def spendHalfMixBox(y: BigInt, halfMixBoxId: String, inputBoxIds: Array[String], feeAmount: Long, changeAddress: String, proverDlogSecrets: Array[String], broadCast: Boolean) = {
    usingClient{implicit ctx =>
      val bob = new BobImpl(y.bigInteger)
      val halfMixBox: InputBox = ctx.getBoxesById(halfMixBoxId)(0)
      val dlogs: Array[BigInteger] = proverDlogSecrets.map(BigInt(_).bigInteger)
      val (fullMixTx, bit) = bob.spendHalfMixBox(HalfMixBox(halfMixBox), inputBoxIds, feeAmount, changeAddress, dlogs, Array[DHT]())
      if (broadCast) ctx.sendTransaction(fullMixTx.tx)
      (fullMixTx, bit)
    }
  }
}
