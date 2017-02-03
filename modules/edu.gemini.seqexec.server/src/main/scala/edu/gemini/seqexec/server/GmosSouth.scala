package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.gmos.InstGmosSouth.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scalaz.{EitherT, Reader}
import scalaz.concurrent.Task

object GmosSouth extends Instrument {

  override val name: String = INSTRUMENT_NAME_PROP

  override val sfName: String = "gmos"

  val Log = Logger.getLogger(getClass.getName)

  var imageCount = 0

  override def configure(config: Config): SeqAction[ConfigResult] = EitherT(Task {
    val items = config.getAll(INSTRUMENT_KEY).itemEntries()

    //    Log.log(Level.INFO, "Configuring " + name + " with :" + ItemEntryUtil.showItems(items))
    Log.log(Level.INFO, "Configuring " + name)
    Thread.sleep(2000)
    Log.log(Level.INFO, name + " configured")

    TrySeq(ConfigResult(this))
  })

  override def observe(config: Config): SeqObserve[(DhsClient, List[Header]), ObserveResult] = Reader { case (dhs, _) =>
    for {
      id <- dhs.createImage(DhsClient.ImageParameters(DhsClient.Permanent, List("gmos", "dhs-http")))
      _ <- EitherT(Task {
             Log.log(Level.INFO, name + ": starting observation " + id)
             Thread.sleep(5000)
             Log.log(Level.INFO, name + ": observation completed")
             TrySeq(())
           })
      _ <- dhs.setKeywords(id, DhsClient.KeywordBag(
             DhsClient.StringKeyword("instrument", "gmos"),
             DhsClient.Int32Keyword("INPORT", 3),
             DhsClient.DoubleKeyword("WAVELENG", 3.14159),
             DhsClient.BooleanKeyword("PROP_MD", value = true)
           ))
    } yield ObserveResult(id)
  }

}
