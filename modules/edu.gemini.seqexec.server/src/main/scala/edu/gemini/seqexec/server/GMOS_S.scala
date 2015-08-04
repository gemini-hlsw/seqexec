package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.InstGmosSouth.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scalaz.concurrent.Task

/**
 * Created by jluhrs on 4/27/15.
 */
object GMOS_S extends Instrument {

  override val name: String = INSTRUMENT_NAME_PROP

  override val sfName: String = "gmos"

  val Log = Logger.getLogger(getClass.getName)

  var imageCount = 0

  override def configure(config: Config): SeqAction[ConfigResult] = Task {
    val items = config.getAll(INSTRUMENT_KEY).itemEntries()

//    Log.log(Level.INFO, "Configuring " + name + " with :" + ItemEntryUtil.showItems(items))
    Log.log(Level.INFO, "Configuring " + name)
    Thread.sleep(2000)
    Log.log(Level.INFO, name + " configured")

    TrySeq(ConfigResult(this))
  }

  override def observe(config: Config): SeqAction[ObserveResult] = Task {
    Log.log(Level.INFO, name + ": starting observation")
    Thread.sleep(5000)
    Log.log(Level.INFO, name + ": observation completed")

    imageCount += 1
    TrySeq(ObserveResult(f"S20150519S$imageCount%04d"))
  }
}
