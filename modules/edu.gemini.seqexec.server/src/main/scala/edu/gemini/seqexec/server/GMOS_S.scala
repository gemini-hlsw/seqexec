package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.InstGmosSouth.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by jluhrs on 4/27/15.
 */
object GMOS_S extends Instrument {

  override val name: String = INSTRUMENT_NAME_PROP

  val Log = Logger.getLogger(getClass.getName)

  override def configure(config: Config): Future[Unit] = Future {

    val items = config.getAll(INSTRUMENT_KEY).itemEntries()

    Thread.sleep(2000)
    Log.log(Level.INFO, name + " configured :" + ItemEntryUtil.showItems(items))
  }

  override def observe(config: Config): Future[Unit] = Future {
    Log.log(Level.INFO, name + ": starting observation")
    Thread.sleep(5000)
    Log.log(Level.INFO, name + ": observation completed")
  }
}
