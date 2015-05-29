package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.seqexec.server._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.seqcomp.SeqConfigNames.{TELESCOPE_CONFIG_NAME, TELESCOPE_KEY}

import scalaz.EitherT
import scalaz.concurrent.Task

/**
 * Created by jluhrs on 4/23/15.
 */
object TCS extends System {
  override val name: String = TELESCOPE_CONFIG_NAME
  private val Log = Logger.getLogger(getClass.getName)

  override def configure(config: Config): Task[ConfigResult] = Task {
    val items = config.getAll(TELESCOPE_KEY).itemEntries()

    Log.log(Level.INFO, "Configuring TCS with:" + ItemEntryUtil.showItems(items))
    Thread.sleep(2000)
    Log.log(Level.INFO, "TCS configured:")

    ConfigResult(this)
  }
}
