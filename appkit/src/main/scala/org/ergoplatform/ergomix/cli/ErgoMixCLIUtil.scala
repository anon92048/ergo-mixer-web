package org.ergoplatform.ergomix.cli

import org.ergoplatform.appkit.{BlockchainContext, ErgoClient}
import org.ergoplatform.ergomix.mixer.BlockExplorer
import org.ergoplatform.ergomix.{AliceImpl, BobImpl, ErgoMix, Util => EUtil}

object ErgoMixCLIUtil {

  var optClient:Option[ErgoClient] = None

  implicit def stringToBigInteger(s:String) = BigInt(s).bigInteger

  val emptyArr = Array[String]()

  def usingClient[T](f: BlockchainContext => T): T= {
    optClient.fold(throw new Exception("Set client first")){client =>
      client.execute{ctx =>
        f(ctx)
      }
    }
  }

  def getUnspentBoxes(address:String) = {
    usingClient{implicit ctx =>
      val explorer = new BlockExplorer()
      explorer.getUnspentBoxes(address)
    }
  }

  def getConfirmationsForBoxId(boxId:String) = {
    usingClient{implicit ctx =>
      val explorer = new BlockExplorer()
      explorer.getConfirmationsForBoxId(boxId)
    }
  }

  def getHalfMixBoxes = { // TODO: take amount as parameter
    usingClient{implicit ctx =>
      val address = new EUtil().getAddressFromProposition(new ErgoMix(ctx).halfMixContract.getErgoTree).toString
      getUnspentBoxes(address).filter(_.amount == ErgoMix.mixAmount) // to ensure we don't cause exception when using wrong amount half-mix box
    }
  }

  def getFullMixBoxes = {
    usingClient{implicit ctx =>
      val address = new EUtil().getAddressFromProposition(new ErgoMix(ctx).fullMixScriptContract.getErgoTree).toString
      getUnspentBoxes(address).filter(_.amount == ErgoMix.mixAmount)
    }
  }

  def getUnspentBoxById(boxId:String) = {
    usingClient{implicit ctx =>
      ctx.getBoxesById(boxId).headOption.getOrElse(throw new Exception("No box found"))
    }
  }

  def getSpendingTxId(boxId:String) = {
    usingClient{implicit ctx =>
      val explorer = new BlockExplorer()
      explorer.getSpendingTxId(boxId)
    }
  }

  def getFeeEmissionBoxes = {
    usingClient{implicit ctx =>
      getUnspentBoxes(new EUtil().getAddressFromProposition(new ErgoMix(ctx).feeEmissionContract.getErgoTree).toString).filter(_.registers.get("R4").isDefined)
    }
  }

  case class Arg(key:String, value:String)

  def parseArgsNoSecret(args:Array[String]) = {
    implicit val l: Seq[Arg] = args.sliding(2, 2).toList.collect {
      case Array(key, value) => Arg(key, value)
    }
    val url = try getArg("url") catch {case a:Throwable => defaultUrl}
    val mainNet: Boolean = (try getArg("mainNet") catch {case a:Throwable => "true"}).toBoolean
    Client.setClient(url, mainNet, None)
    l
  }

  def parseArgs(args:Array[String]): (Seq[Arg], BigInt) = {
    implicit val l: Seq[Arg] = args.sliding(2, 2).toList.collect {
      case Array(key, value) => Arg(key, value)
    }
    val url = try getArg("url") catch {case a:Throwable => defaultUrl}
    val mainNet: Boolean = (try getArg("mainNet") catch {case a:Throwable => "true"}).toBoolean
    val secret = BigInt(getArg("secret"), 10)
    Client.setClient(url, mainNet, None)
    (l, secret)
  }

  def getArgs(key:String)(implicit args:Seq[Arg]):Seq[String] = args.filter(_.key == "--"+key).map(_.value) match {
    case Nil => throw new Exception(s"Argument $key missing")
    case any => any
  }
  def getArg(key:String)(implicit args:Seq[Arg]):String = getArgs(key) match {
    case List(arg) => arg
    case _ => throw new Exception(s"Multiple $key arguments")
  }

  val defaultUrl = "http://88.198.13.202:9053/"
  def getProver(secret:BigInt, isAlice:Boolean)(implicit ctx:BlockchainContext) = if (isAlice) new AliceImpl(secret.bigInteger) else new BobImpl(secret.bigInteger)
  def isAlice(implicit args:Seq[Arg]) = getArg("mode") match{
    case "alice" => true
    case "bob" => false
    case any => throw new Exception(s"Invalid mode $any")
  }
}
