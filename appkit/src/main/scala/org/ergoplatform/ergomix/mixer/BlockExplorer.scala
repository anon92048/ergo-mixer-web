package org.ergoplatform.ergomix.mixer

import io.circe.Json
import org.ergoplatform.appkit.BlockchainContext
import org.ergoplatform.ergomix.mixer.Models.{InBox, OutBox, SpendTx}
import org.ergoplatform.ergomix.{ErgoMix, Util => EUtil}

class BlockExplorer(implicit ctx:BlockchainContext) extends UTXOReader {

  // private val baseUrl = "https://api.ergoplatform.com"
  private val baseUrl = "https://new-explorer.ergoplatform.com"

  val unspentUrl = s"$baseUrl/transactions/boxes/byAddress/unspent/"
  val blockUrl = s"$baseUrl/blocks"
  val boxUrl = s"$baseUrl/transactions/boxes/"
  val txUrl = s"$baseUrl/transactions/"

  private val ergoMix = new ErgoMix(ctx)

  private val util = new EUtil

  private def getOutBoxFromJson(j: Json) = {
    val id = getId(j)
    val value = (j \\ "value").map(v => v.asNumber.get).apply(0)
    val registers = (j \\ "additionalRegisters").flatMap{r =>
      r.asObject.get.toList.map{
        case (key, value) => (key, value.asString.get)
      }
    }.toMap
    val ergoTree = (j \\ "ergoTree").map(v => v.asString.get).apply(0)
    val spendingTxId = (j \\ "spentTransactionId").map(v => v.asString).apply(0)
    OutBox(id, value.toLong.get, registers, ergoTree, spendingTxId)
  }

  private def getId(j:Json) = (j \\ "id").map(v => v.asString.get).apply(0)

  private def getInBoxFromJson(j: Json) = {
    val id = getId(j)
    val address = (j \\ "address").map(v => v.asString.get).apply(0)
    val value = (j \\ "value").map(v => v.asNumber.get).apply(0)
    val createdTxId = (j \\ "outputTransactionId").map(v => v.asString.get).apply(0)
    InBox(id, address, createdTxId, value.toLong.get)
  }

  private def getOutBoxesFromJson(json:Json): Seq[OutBox] = {
    json.asArray.get.map(getOutBoxFromJson)
  }

  private def getInBoxesFromJson(json:Json): Seq[InBox] = {
    json.asArray.get.map(getInBoxFromJson)
  }

  def getHeight: Int = {
    val json = GetURL.get(blockUrl)
    ((json \\ "items")(0).asArray.get(0) \\ "height")(0).toString().toInt
  }

  override def getUnspentBoxes(address:String):Seq[OutBox] = {
    getOutBoxesFromJson(GetURL.get(unspentUrl + address))
  }

  def getBoxById(boxId:String) = {
    getOutBoxFromJson(GetURL.get(boxUrl + boxId))
  }

  def getSpendingTxId(boxId: String) = try {
    getBoxById(boxId).spendingTxId
  } catch {
    case a:Throwable => None
  }

  def getConfirmationsForBoxId(boxId: String) = try {
    val json = (GetURL.get(boxUrl + boxId))
    (json \\ "creationHeight").headOption.map{creationHeight =>
      val height = getHeight
      height - creationHeight.asNumber.get.toInt.get
    }.getOrElse(0)
  } catch { case any:Throwable => 0}

  def getTransaction(txId: String): Option[SpendTx] = try {
    val json = (GetURL.get(txUrl + txId))
    val outputs = (json \\ "outputs").headOption.get
    val inputs = (json \\ "inputs").headOption.get
    Some(SpendTx(getInBoxesFromJson(inputs), getOutBoxesFromJson(outputs)))
  } catch {
    case a:Throwable => None
  }
}

object GetURL {
  import scala.io.Source
  import java.net.URL
  import io.circe._, io.circe.parser._

  val requestProperties = Map(
    "User-Agent" -> "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)"
  )
  def get(url:String) = {
    val connection = new URL(url).openConnection
    requestProperties.foreach({
      case (name, value) => connection.setRequestProperty(name, value)
    })
    val jsonStr = Source.fromInputStream(connection.getInputStream).getLines.mkString("\n")
    parse(jsonStr).right.get
  }
}