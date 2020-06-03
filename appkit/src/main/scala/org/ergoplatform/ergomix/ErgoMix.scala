package org.ergoplatform.ergomix

import org.ergoplatform.appkit._
import org.ergoplatform.ergomix.mixer.ErgoMixerUtil
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder}
import sigmastate.Values.ErgoTree
import sigmastate.eval._
import sigmastate.interpreter.CryptoConstants
import special.sigma.GroupElement

class Util(implicit ctx:BlockchainContext) {
  val addressEncoder = new ErgoAddressEncoder(ctx.getNetworkType.networkPrefix)
  def getAddress(address: String): ErgoAddress = addressEncoder.fromString(address).get
  def getAddressFromProposition(ergoTree: ErgoTree): ErgoAddress = addressEncoder.fromProposition(ergoTree).get
}

object ErgoMix {
  //  val mixAmount = 1000000000L // in NanoErgs, = 1 Erg
  val mixAmount = 1500000L // nanoErgs
  val feeAmount = 1500000L // NanoErgs
  def getHash(bytes:Array[Byte]) = scorex.crypto.hash.Blake2b256(bytes)
  val g: GroupElement = CryptoConstants.dlogGroup.generator
  def hexToGroupElement(hex:String):GroupElement = {
    JavaHelpers.decodeStringToGE(hex)
  }

  def fullMixEndBox(fullMixScriptTree:ErgoTree, r4:GroupElement, r5:GroupElement, r6:GroupElement, value:Long, tokens:Seq[Token]) = {
    EndBox(fullMixScriptTree, Seq(ErgoValue.of(r4), ErgoValue.of(r5), ErgoValue.of(r6)), value, tokens)
  }

  def fullOutboxBuilder(outBoxBuilder:OutBoxBuilder)(value:Long, r4:GroupElement, r5:GroupElement, r6:GroupElement, ergoTokens:Seq[Token], fullMixScriptContract:ErgoContract) = {
    outBoxBuilderWithTokens(
      outBoxBuilder.value(
        value
      ).registers(
        ErgoValue.of(r4), ErgoValue.of(r5), ErgoValue.of(r6)
      ).contract(
        fullMixScriptContract
      )
    )(ergoTokens)
  }

  def outBoxBuilderWithTokens(outBoxBuilder: OutBoxBuilder)(tokens:Seq[Token]) = {
    if (tokens.isEmpty) outBoxBuilder else {
      outBoxBuilder.tokens(
        tokens.map(token => token.toErgoToken): _*
      )
    }
  }
}

import org.ergoplatform.ergomix.ErgoMix._

class ErgoMix(ctx:BlockchainContext) {
  val fullMixScriptContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.empty(),
    """{
      |  val g = groupGenerator
      |  val c1 = SELF.R4[GroupElement].get
      |  val c2 = SELF.R5[GroupElement].get
      |  val gX = SELF.R6[GroupElement].get
      |  proveDlog(c2) ||          // either c2 is g^y
      |  proveDHTuple(g, c1, gX, c2) // or c1 is g^y and c2 = gX^y = g^xy
      |}""".stripMargin
  )

  val fullMixScriptErgoTree = fullMixScriptContract.getErgoTree

//  val fullMixScriptHash = getHash(fullMixScriptErgoTree.bytes)

  val halfMixContract = ctx.compileContract(
    ConstantsBuilder.create().item(
     "fullMixScript", fullMixScriptErgoTree.bytes
    ).build(),
    """{
      |  val g = groupGenerator
      |  val gX = SELF.R4[GroupElement].get
      |
      |  val c1 = OUTPUTS(0).R4[GroupElement].get
      |  val c2 = OUTPUTS(0).R5[GroupElement].get
      |  val u1 = OUTPUTS(0).R6[GroupElement].get
      |  val u2 = OUTPUTS(1).R6[GroupElement].get
      |
      |  sigmaProp(
      |    OUTPUTS(0).value == SELF.value &&
      |    OUTPUTS(1).value == SELF.value &&
      |    OUTPUTS(0).tokens == SELF.tokens &&
      |    OUTPUTS(1).tokens == SELF.tokens &&
      |    u1 == gX && u2 == gX &&
      |    OUTPUTS(0).propositionBytes == fullMixScript &&
      |    OUTPUTS(1).propositionBytes == fullMixScript &&
      |    OUTPUTS(1).R4[GroupElement].get == c2 &&
      |    OUTPUTS(1).R5[GroupElement].get == c1 &&
      |    SELF.id == INPUTS(0).id && c1 != c2
      |  ) && {
      |    proveDHTuple(g, gX, c1, c2) ||
      |    proveDHTuple(g, gX, c2, c1)
      |  }
      |}""".stripMargin
  )

  val halfMixScriptHash = getHash(halfMixContract.getErgoTree.bytes)

  /*
  The above provides a very basic mechanism of handling fee.
  The complete solution would combine two approaches:
   1. implement fee using a token as described in advanced ergoscript tutorial
   2. implement a fee emission box that can be exchanged with the tokens when spending a full mix box (as explained in forum post)

  However, here the solution is much simpler. It is a variation of the above idea except that step 1 is skipped
  The fee emission box does not require tokens and can emit fee whenever one of the input boxes in the transaction is a full mix box.

  There should be multiple instances of fee emission box to ensure multiple people can use them in the same transaction.
  There is also the issue of avoiding collisions (when two people try to spend the same fee box)

  * */
  val feeEmissionContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create().item(
      "fullMixScript", fullMixScriptErgoTree.bytes
    ).item(
      "halfMixScriptHash", halfMixScriptHash
    ).item(
      "feeAmount", feeAmount
    ).build(),
    /* Fee emission box can only be used in spending a full mix box.
    Spending half-mix box can be done if the other input is a full-mix box (see below)

       We can spend a full mix box in two ways:

        1. fee emission box as input #0  |--> external address or half mix box at output #0
           full mix box as input #1      |    fee box at output #1
                                         |    fee emission box at output #2 (change)

        2. half mix box as input #0      |--> full mix box at output #0
           fee emission box as input #1  |    full mix box at output #1
           full mix box as input #2      |    fee box at output #2
                                         |    fee emission box at output #3 (change)
     */
    """
      |{
      |  val fullMixBoxAtInput = {(i:Int) => INPUTS(i).propositionBytes == fullMixScript }
      |
      |  val halfMixBox = {(b:Box) => blake2b256(b.propositionBytes) == halfMixScriptHash }
      |
      |  val feeEmitBoxAtInput = {(i:Int) => INPUTS(i).id == SELF.id }
      |
      |  val gZ = SELF.R4[GroupElement].get
      |  val feeEmitBoxAtOutput = {(i:Int) =>
      |    OUTPUTS(i).propositionBytes == SELF.propositionBytes &&
      |    OUTPUTS(i).value == SELF.value - feeAmount &&
      |    OUTPUTS(i).R4[GroupElement].get == gZ
      |  }
      |
      |  sigmaProp(
      |    (fullMixBoxAtInput(1) && feeEmitBoxAtInput(0) && feeEmitBoxAtOutput(2) && halfMixBox(OUTPUTS(0))) ||
      |    (fullMixBoxAtInput(2) && feeEmitBoxAtInput(1) && feeEmitBoxAtOutput(3) && halfMixBox(INPUTS(0)))
      |  ) || proveDlog(gZ)
      |}
      |""".stripMargin
  )
}
