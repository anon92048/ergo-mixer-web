package org.ergoplatform.ergomix.mixer

import java.io.InputStream
import java.net.HttpURLConnection

import io.circe.Json
import org.ergoplatform.appkit.BlockchainContext
import org.ergoplatform.ergomix.mixer.Models.{InBox, OutBox, SpendTx}
import org.ergoplatform.ergomix.{ErgoMix, Util => EUtil}

import scala.util.{Failure, Success, Try}

class BlockExplorer(implicit ctx:BlockchainContext) extends UTXOReader {

  // private val baseUrl = "https://api.ergoplatform.com"
  private val baseUrl = "https://new-explorer.ergoplatform.com"

  private val unspentUrl = s"$baseUrl/transactions/boxes/byAddress/unspent/"
  private val blockUrl = s"$baseUrl/blocks"
  private val boxUrl = s"$baseUrl/transactions/boxes/"
  private val txUrl = s"$baseUrl/transactions/"

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

  def doesBoxExist(boxId: String) = {
    GetURL.getOrError(boxUrl + boxId) match {
      case Right(Some(_)) => Some(true)
      case Right(None) => Some(false)
      case Left(ex) => None
    }
  }

  def getTransaction(txId: String): Option[SpendTx] = try {
    val json = (GetURL.get(txUrl + txId))
    val outputs = (json \\ "outputs").headOption.get
    val inputs = (json \\ "inputs").headOption.get
    Some(SpendTx(getInBoxesFromJson(inputs), getOutBoxesFromJson(outputs), txId))
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

  private def is2Str(is:InputStream) = {
    Try(Source.fromInputStream(is).getLines.mkString("\n")) match {
      case Success(s) => s
      case Failure(exception) => exception.getMessage
    }
  }

  def getOrError(url:String) = {
    Try {
      val connection = new URL(url).openConnection
      requestProperties.foreach{case (name, value) => connection.setRequestProperty(name, value)}
      val httpConn = connection.asInstanceOf[HttpURLConnection]
      (httpConn.getResponseCode, httpConn)
    } match {
      case Success((200, httpConn)) => Try(Some(parse(is2Str(httpConn.getInputStream)).right.get)).toEither
      case Success((404, _)) => Right(None) // not found; we want to consider this as a "good" case (implies box has 0 confirmation or does not exist)
      case Success((httpCode, httpConn)) => Left(new Exception(s"http:$httpCode,error:${is2Str(httpConn.getErrorStream)}"))
      case Failure(ex) => Left(ex)
    }
  }

  def get(url:String) = {
    getOrError(url) match {
      case Right(Some(json)) => json
      case Right(None) => throw new Exception("Explorer returned error 404")
      case Left(ex) => throw ex
    }
  }
}