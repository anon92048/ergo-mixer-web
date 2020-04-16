package services

import org.ergoplatform.ergomix.cli.Client
import org.ergoplatform.ergomix.db.config.DBConfig
import org.ergoplatform.ergomix.mixer.{ErgoMixer, ErgoMixerJobs, Tables}

object ErgoMixingSystem {

  object Config extends DBConfig {
    override val dbname: String = "ergo_mixer"
    override val dbuser: String = "ergo_mixer"
    override val dbpass: String = "2l93sQWzd1f4esk5w"
  }


  val tables = new Tables(Config)

  val ergoMixer = new ErgoMixer(tables)
  val ergoMixerJobs = new ErgoMixerJobs(tables)

  Client.setClient("http://88.198.13.202:9053/", isMainnet = true, None)
}
