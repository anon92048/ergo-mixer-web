package services

import java.text.SimpleDateFormat
import java.util.Date

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import models.RingStats
import org.ergoplatform.ergomix.cli.ErgoMixCLIUtil
import org.ergoplatform.ergomix.mixer.ErgoMixerJobs
import services.ScheduledJobs.{RefreshMixingStats, RefreshPoolStats}

import scala.util.Try


class ScheduledJobs(mixerJobs: ErgoMixerJobs) extends Actor with ActorLogging {

  private def currentTimeString = {
    val date = new Date()
    val formatter = new SimpleDateFormat("HH:mm:ss")
    formatter.format(date)
  }

  override def receive: Receive = {
    case RefreshMixingStats =>
      println(s"$currentTimeString: Refreshing mixing stats: jobs started")
      println("rescan: " + Try(mixerJobs.rescanAllMixes))
      println("deposits: " + Try(mixerJobs.processDeposits))
      println("new mixes: " + Try(mixerJobs.processNewMixQueue))
      println("full mixes: " + Try(mixerJobs.processFullMixQueue))
      println("half mixes: " + Try(mixerJobs.processHalfMixQueue))
      println(s"$currentTimeString: Refreshing mixing stats: jobs finished")
      // read mixing statuses table here and process each (unfinished) record based on its status

    case RefreshPoolStats =>
      println(s"$currentTimeString: Refreshing pool stats: jobs started")
      RingStats.halfMixBoxesCount = Some(ErgoMixCLIUtil.getHalfMixBoxes.size)
      RingStats.fullMixBoxesCount = Some(ErgoMixCLIUtil.getFullMixBoxes.size)
      RingStats.emissionBoxesCount = Some(ErgoMixCLIUtil.getFeeEmissionBoxes.size)
      println(s"$currentTimeString: Refreshing pool stats: jobs finished")
  }
}

object ScheduledJobs {

  def props(mixerJobs: ErgoMixerJobs)(implicit system: ActorSystem): Props =
    Props.create(classOf[ScheduledJobs], mixerJobs)

  case object RefreshMixingStats
  case object RefreshPoolStats
}
