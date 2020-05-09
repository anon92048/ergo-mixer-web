package org.ergoplatform.ergomix.mixer

class ErgoMixerJobs(tables:Tables) {
  val ergoMixer = new ErgoMixer(tables)
  val rescan = new Rescan(tables)
  val halfMixer = new HalfMixer(tables)
  val fullMixer = new FullMixer(tables)
  val newMixer = new NewMixer(tables)
  val deposits = new Deposits(tables)
}
