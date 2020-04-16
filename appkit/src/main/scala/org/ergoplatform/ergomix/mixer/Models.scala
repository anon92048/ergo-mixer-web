package org.ergoplatform.ergomix.mixer


import java.util
import java.util.Date

import io.circe.{Encoder, Json, parser}
import io.circe.generic.auto._
import io.circe.syntax._
import org.ergoplatform.appkit.{ErgoId, ErgoToken, ErgoValue, InputBox}
import org.ergoplatform.ergomix.ErgoMix
import org.ergoplatform.ergomix.{Util => EUtil}
import org.ergoplatform.ergomix.cli.ErgoMixCLIUtil
import org.ergoplatform.ergomix.db.core.DataStructures.anyToAny
import scorex.util.encode.Base16
import sigmastate.Values
import sigmastate.serialization.ErgoTreeSerializer
import special.sigma.GroupElement

import scala.jdk.CollectionConverters._

object Models {

  //  case class OutBox(boxId:String, address:String)
  //    {override def toString = this.asJson.toString}

  case class SpendTx(inboxes:Seq[InBox], outboxes:Seq[OutBox])

  case class InBox(id:String, address:String, createdTxId:String, value:Long) {
    def isHalfMixBox: Boolean = ErgoMixCLIUtil.usingClient{ implicit ctx =>
      val halfMixBoxAddress = new EUtil().getAddressFromProposition(new ErgoMix(ctx).halfMixContract.getErgoTree).toString
      address == halfMixBoxAddress
    }
  }

  case class OutBox(id:String, amount:Long, registers:Map[String, String], ergoTree:String, spendingTxId:Option[String]){
    def ge(regId:String) = ErgoMix.hexToGroupElement(registers.get(regId).get.drop(2))
    def mixBox: Option[Either[HBox, FBox]] = try ErgoMixCLIUtil.usingClient{ implicit ctx =>
      val ergoMix = new ErgoMix(ctx)
      val fullMixBoxErgoTree = Base16.encode(ergoMix.fullMixScriptErgoTree.bytes).trim
      val halfMixBoxErgoTree = Base16.encode(ergoMix.halfMixContract.getErgoTree.bytes).trim
      ergoTree match {
        case `fullMixBoxErgoTree` =>
          Some(Right(FBox(id, ge("R4"), ge("R5"), ge("R6"))))
        case `halfMixBoxErgoTree` =>
          Some(Left(HBox(id, ge("R4"))))
        case any =>
          None
      }
    } catch {
      case a:Throwable =>
        a.printStackTrace()
        None
    }

    def getFBox = mixBox.flatMap{
      case Right(fBox) => Some(fBox)
      case _ => None
    }

    def getHBox = mixBox.flatMap{
      case Left(hBox) => Some(hBox)
      case _ => None
    }

    override def toString = this.asJson.toString
  }

  sealed abstract class MixStatus(val value:String)

  implicit val encodeMixStatus: Encoder[MixStatus] = new Encoder[MixStatus] {
    final def apply(a: MixStatus): Json = Json.fromString(a.value)
  }

  object MixStatus {
    object Queued extends MixStatus("queued")
    object Running extends MixStatus("running")
    object Complete extends MixStatus("complete")
    private def all = Seq(Queued, Running, Complete)
    def fromString(s:String) = all.find(_.value == s).getOrElse(throw new Exception(s"Invalid status $s"))
  }

  case class MixRequest(id:String, amount:Long, numRounds:Int, mixStatus: MixStatus, createdTime:Long, withdrawAddress:String, depositAddress:String, depositCompleted:Boolean)
    {
      def creationTimePrettyPrinted: String = {
        import java.text.SimpleDateFormat
        import java.util.TimeZone

        val date = new Date(createdTime)
        val formatter = new SimpleDateFormat("HH:mm:ss")
        formatter.format(date)
      }

      override def toString = this.asJson.toString
    }

  object MixRequest{
    def apply(a:Array[Any]) = {
      val i = a.toIterator
      new MixRequest(
        i.next().as[String],
        i.next().as[Long],
        i.next().as[Int],
        MixStatus.fromString(i.next().as[String]),
        i.next().as[Long],
        i.next().as[String], // withdraw address
        i.next().as[String],  // deposit address
        i.next().as[Boolean]
      )
    }
  }

  implicit val encodeMixState: Encoder[MixState] = new Encoder[MixState] {
    final def apply(a: MixState): Json = {
      Json.obj(
        "isAlice" -> Json.fromBoolean(a.isAlice),
        "round" -> Json.fromInt(a.round)
      )
    }
  }

  case class MixState(id:String, round:Int, isAlice:Boolean)
    {override def toString = this.asJson.toString}

  object MixState{
    def apply(a:Array[Any]) = {
      val i = a.toIterator
      new MixState(
        i.next().as[String],
        i.next().as[Int],
        i.next().as[Boolean]
      )
    }
  }

  case class MixHistory(id:String, round:Int, isAlice:Boolean, time:Long)
    {override def toString = this.asJson.toString}

  object MixHistory{
    def apply(a:Array[Any]) = {
      val i = a.toIterator
      new MixHistory(
        i.next().as[String],
        i.next().as[Int],
        i.next().as[Boolean],
        i.next().as[Long]
      )
    }
  }

  case class Deposit(address: String, boxId:String, amount:Long, createdTime:Long)
    {override def toString = this.asJson.toString}

  object Deposit {
    def apply(a:Array[Any]) = {
      val i = a.toIterator
      new Deposit(
        i.next().as[String],
        i.next().as[String],
        i.next().as[Long],
        i.next().as[Long]
      )
    }
  }

  implicit val encodeHalfMix: Encoder[HalfMix] = new Encoder[HalfMix] {
    final def apply(a: HalfMix): Json = {
      Json.obj(
        "mixId" -> Json.fromString(a.mixId),
        "round" -> Json.fromInt(a.round),
        "halfMixBoxId" -> Json.fromString(a.halfMixBoxId),
        "createdTime" -> Json.fromLong(a.createdTime),
        "age" -> Json.fromString(s"${(System.currentTimeMillis() - a.createdTime)/(1000*60)} minutes")
      )
    }
  }

  case class HalfMix(mixId:String, round:Int, createdTime:Long, halfMixBoxId:String, isSpent:Boolean)
    {override def toString = this.asJson.toString}

  object HalfMix{
    def apply(a:Array[Any]) = {
      val i = a.toIterator
      new HalfMix(
        i.next().as[String],
        i.next().as[Int],
        i.next().as[Long],
        i.next().as[String],
        i.next().as[Boolean]
      )
    }
  }

  implicit val encodeFullMix: Encoder[FullMix] = new Encoder[FullMix] {
    final def apply(a: FullMix): Json = {
      Json.obj(
        "mixId" -> Json.fromString(a.mixId),
        "round" -> Json.fromInt(a.round),
        "halfMixBoxId" -> Json.fromString(a.halfMixBoxId),
        "fullMixBoxId" -> Json.fromString(a.fullMixBoxId),
        "createdTime" -> Json.fromLong(a.createdTime),
        "age" -> Json.fromString(s"${(System.currentTimeMillis() - a.createdTime)/(1000*60)} minutes")
      )
    }
  }

  case class FullMix(mixId:String, round:Int, createdTime:Long, halfMixBoxId:String, fullMixBoxId:String)
    {override def toString = this.asJson.toString}

  object FullMix{
    def apply(a:Array[Any]) = {
      val i = a.toIterator
      new FullMix(
        i.next().as[String],
        i.next().as[Int],
        i.next().as[Long],
        i.next().as[String],
        i.next().as[String]
      )
    }
  }

  implicit val encodeWithdraw: Encoder[Withdraw] = new Encoder[Withdraw] {
    final def apply(a: Withdraw): Json = {
      Json.obj(
        "txId" -> Json.fromString(a.txId),
        "createdTime" -> Json.fromLong(a.createdTime)
      )
    }
  }

  case class Withdraw(mixId:String, txId:String, createdTime:Long, fullMixBoxId:String, tx:Json)
    {override def toString = this.asJson.toString}

  object Withdraw{
    def apply(a:Array[Any]) = {
      val i = a.toIterator
      new Withdraw(
        i.next().as[String],
        i.next().as[String],
        i.next().as[Long],
        i.next().as[String],
        parser.parse(new String(i.next().as[Array[Byte]], "utf-16")).right.get
      )
    }
  }

  case class Mix(mixRequest: MixRequest, mixState: Option[MixState], halfMix: Option[HalfMix], fullMix: Option[FullMix], withdraw: Option[Withdraw])
    {override def toString = this.asJson.toString}

  // for scanning blockchain
  case class FBox(id:String, r4:GroupElement, r5:GroupElement, r6:GroupElement)
  case class HBox(id:String, r4:GroupElement)

}
