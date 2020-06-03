package org.ergoplatform.ergomix

import org.ergoplatform.appkit._
import sigmastate.eval._
import sigmastate.interpreter.CryptoConstants
import special.sigma.GroupElement

object MyParams {
  val otherAddress = "9f5ZKbECVTm25JTRQHDHGM5ehC8tUw5g1fCBQ4aaE792rWBFrjK" // address corresponding to otherDlogSecret below

  val g: GroupElement = CryptoConstants.dlogGroup.generator

  // for spending from otherAddress (which is the other input in our transaction). This is used to pay fee and receive change.
  // Later on this will be replaced by the "Fee Emission box" as described in this post: https://www.ergoforum.org/t/paying-fee-in-ergomix-in-primary-tokens/73
  val mySuperTopSecret: scala.math.BigInt = scala.math.BigInt("12345") // actual secret replaced with 12345. This secret must correspond to otherAddress
  // mySuperTopSecret = BigInt("62283677737102927456080635374095232205112173881899107864112332574434059604278").bigInteger
  // x =                BigInt("24561684168165411265341651231535167365451734571355618735546417863313522331516").bigInteger
  // y =                BigInt("38217912379856723498519009226514665321236253235155646299754312254269416362526").bigInteger
  // nextX =            BigInt("78989182198401984091793246139851983471364719384103947913654797813798247932172").bigInteger

  val otherDlogSecret = mySuperTopSecret.bigInteger // corresponding to otherAddress
  val otherGroupElement:GroupElement = g.exp(otherDlogSecret)

  val x = (mySuperTopSecret - 299).bigInteger // Alice's secret
  val y = (mySuperTopSecret - 399).bigInteger // Bob's secret

  val carolSecret = BigInt("986417816238909341920901841947198471984714714714134145125").bigInteger // we will pay to Carol when spending the full mix boxes
  val carolGroupElement:GroupElement = g.exp(carolSecret)

  def payToGroupElement(ctx:BlockchainContext, gZ: GroupElement) =
    ctx.compileContract(ConstantsBuilder.create().item("gZ", gZ).build(),"{proveDlog(gZ)}")

}

import org.ergoplatform.ergomix.MyParams._

object CreateHalfMixBox extends App {
  val client = RestApiErgoClient.create("http://88.198.13.202:9053/", NetworkType.MAINNET, "")
  val otherBoxId = "4b10b518b6e95d1c2a862b5e73a8258a7817a16f905df1292487dd7ce9e2c0ed"

  client.execute{implicit ctx:BlockchainContext =>
    val alice = new AliceImpl(x)
    val halfMixTx = alice.createHalfMixBox(Array(otherBoxId), 1500000L, otherAddress, Array(otherDlogSecret), Array[DHT]())
    println(halfMixTx.tx.toJson(false)) // broadcast what is printed
  }
}

object CreateFullMixBox extends App {
  val client = RestApiErgoClient.create("http://88.198.13.202:9053/", NetworkType.MAINNET, "")
  val halfMixBoxId = "f88cd4fad97b37c9da5bb5ae9b38e13e5bf1e4866e0eaf18fa93c8b40d80910c"
  val otherBoxId = "47f77c401b48d48293dc8ec04aaa139f13e2a765fdf746469a2acf491018a62e"
  client.execute{implicit ctx:BlockchainContext =>
    val bob = new BobImpl(y)
    val otherInput = ctx.getBoxesById(otherBoxId)(0)
    val halfMixBox = HalfMixBox(ctx.getBoxesById(halfMixBoxId)(0))
    val (fullMixTx, bit) = bob.spendHalfMixBox(halfMixBox, Array(otherInput), 1500000L, otherAddress, Array(otherDlogSecret), Array[DHT]())
    println(fullMixTx. tx.toJson(false)) // broadcast what is printed
  }
}

object SpendFullMixBoxAlice extends App {
  val client = RestApiErgoClient.create("http://88.198.13.202:9053/", NetworkType.MAINNET, "")
  val fullMixBoxId = "41f32875d4dc9abcd0d2c3751ca03737b0f0782b3c1deaba142232c44593fc8a" // alice's full mix box
  val otherBoxId = "04acfb17f79dc60175ac182a460aa44e36e9e21f227857710ac80ed3f6aa549f"
  client.execute{implicit ctx:BlockchainContext =>
    val alice = new AliceImpl(x)
    val fullfMixBox = FullMixBox(ctx.getBoxesById(fullMixBoxId)(0))
    val endBox = EndBox(payToGroupElement(ctx, carolGroupElement).getErgoTree, Nil, ErgoMix.mixAmount, Nil)
    //spendFullMixBox(f: FullMixBox, endBox: EndBox, feeAmount:Long, otherInputBoxIds:Array[String], changeAddress:String, additionalDlogSecrets:Array[BigInteger], additionalDHTuples:Array[DHT])
    val tx = alice.spendFullMixBox(fullfMixBox, Seq(endBox), 1500000,
      Array(otherBoxId),
      otherAddress, // change address
      Nil,
      Array(otherDlogSecret), Array[DHT]() // for spending box with id otherBoxId
    )
    println(tx.toJson(false)) // broadcast what is printed
  }
}

object SpendFullMixBoxBob extends App {
  val client = RestApiErgoClient.create("http://88.198.13.202:9053/", NetworkType.MAINNET, "")
  val fullMixBoxId = "80b2e74216ba13e4368cf0d23c0d36e2a9b71453cc44450f1403f9a45d56abf9" // bob's full mix box
  val otherBoxId = "2be864ae8e6c3674467881f50a3af19b57457acd307ad400563df2780be138e1"
  client.execute{implicit ctx:BlockchainContext =>
    val bob = new BobImpl(y)
    val fullfMixBox = FullMixBox(ctx.getBoxesById(fullMixBoxId)(0))
    val endBox = EndBox(payToGroupElement(ctx, carolGroupElement).getErgoTree, Nil, ErgoMix.mixAmount, Nil)
    val tx = bob.spendFullMixBox(
      fullfMixBox, Seq(endBox), 1500000,
      Array(otherBoxId),
      otherAddress, // change address
      Nil,
      Array(otherDlogSecret), Array[DHT]()
    )
    println(tx.toJson(false)) // broadcast what is printed
  }
}

object SpendCarolBoxes extends App {
  val client = RestApiErgoClient.create("http://88.198.13.202:9053/", NetworkType.MAINNET, "")
  val boxId0 = "dd6a28fbeeccc23229a9f82d4580f3860c2d528101f7143c2102b08cef0901d8"
  val boxId1 = "9da749e6ba8f1fe741ea607e7b63cf25b033e11ac474606c4923fbbcbc6c5314"
  client.execute { implicit ctx: BlockchainContext =>
    val util = new Util()
    val inputs = new java.util.ArrayList[InputBox]()
    val inBoxes = ctx.getBoxesById(boxId0, boxId1)
    inBoxes.foreach(inputs.add)
    val txB = ctx.newTxBuilder()

    val outputs = txB.outBoxBuilder
      .value(198000000)
      .contract(payToGroupElement(ctx, otherGroupElement)).build()

    val unsigned = txB.boxesToSpend(inputs).outputs(outputs).fee(2000000).sendChangeTo(util.getAddress(otherAddress)).build()
    val prover = ctx.newProverBuilder().withDLogSecret(carolSecret).build()
    val tx = prover.sign(unsigned)
    println(tx.toJson(false)) // broadcast what is printed
  }
}