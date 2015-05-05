package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.seqcomp.SeqConfigNames.{TELESCOPE_CONFIG_NAME, TELESCOPE_KEY}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by jluhrs on 4/23/15.
 */
object TCS extends System {
  override val name: String = TELESCOPE_CONFIG_NAME
  private val Log = Logger.getLogger(getClass.getName)

  override def configure(config: Config): Future[Unit] = Future {
    val items = config.getAll(TELESCOPE_KEY).itemEntries()

    Thread.sleep(200)
    Log.log(Level.INFO, "TCS configured:" + ItemEntryUtil.showItems(items))
  }
}
