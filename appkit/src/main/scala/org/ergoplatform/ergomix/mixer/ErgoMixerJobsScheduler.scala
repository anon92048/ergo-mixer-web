package org.ergoplatform.ergomix.mixer

import akka.actor.ActorSystem

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random


class ErgoMixerJobsScheduler(ergoMixerJobs: ErgoMixerJobs) {

  // job to start queued actions etc

  val initialDelay = 2.minutes
  val jobPeriod: FiniteDuration = 5.minutes

  implicit val ec = ExecutionContext.global

  val actorSystem: ActorSystem = ActorSystem("ergo_mixer")

  def scheduleJob(f: => Future[_], interval: FiniteDuration): Unit = {
    actorSystem.scheduler.scheduleOnce(interval + Random.nextInt(60).seconds) {
      f.onComplete(_ => scheduleJob(f, interval))
    }
  }

  scheduleJob(Future {
//    println("Scheduling job processDeposits")
    ergoMixerJobs.processDeposits
  }, jobPeriod) // checks for pending deposits

  scheduleJob(Future {
//    println("Scheduling job processNewMixQueue")
    ergoMixerJobs.processNewMixQueue
  }, jobPeriod) // checks for pending new mixes

  scheduleJob(Future {
//    println("Scheduling job processRemixQueue")
    ergoMixerJobs.processFullMixQueue
  }, jobPeriod) // checks for pending remixes

  scheduleJob(Future {
//    println("Scheduling job processHalfMixQueue")
    ergoMixerJobs.processHalfMixQueue
  }, jobPeriod) // checks if our half mix box is spent by someone

//  scheduleJob(Future {
//    println("Scheduling job processHalfMixUndoQueue")
//    ergoMixerJobs.rescanAllMixes
//  }, jobPeriod)

  println("JOB SCHEDULER STARTED")
}