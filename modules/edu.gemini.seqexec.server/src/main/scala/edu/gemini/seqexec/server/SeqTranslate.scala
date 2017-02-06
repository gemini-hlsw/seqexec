package edu.gemini.seqexec.server

import edu.gemini.model.p1.immutable.Site
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine.{Action, Result, Sequence, Step}
import edu.gemini.seqexec.model.Model.SequenceMetadata
import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.DhsClient.{KeywordBag, StringKeyword}
import edu.gemini.seqexec.server.SeqexecFailure.UnrecognizedInstrument
import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

/**
  * Created by jluhrs on 9/14/16.
  */
class SeqTranslate(site: Site) {

  import SeqTranslate._

  implicit def toAction(x: SeqAction[Result.Response]): Action = x.run map {
    case -\/(e) => Result.Error(SeqexecFailure.explain(e))
    case \/-(r) => Result.OK(r)
  }

  private def step(systems: Systems, settings: Settings)(obsId: SPObservationID, i: Int, config: Config): SeqexecFailure \/ Step[Action] = {

    def buildStep(inst: Instrument, instHeaders: List[Header]): Step[Action] = {

      val sys = List(Tcs(systems.tcs), inst)
      val headers = List(new StandardHeader(systems.dhs,
        ObsKeywordReaderImpl(config, site.name.replace(' ', '-')),
        if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader
      )) ++ instHeaders

      def observe(config: Config): SeqAction[ObserveResult] = {
        val dataId: SeqAction[String] = EitherT(Task(config.extract(OBSERVE_KEY / DATA_LABEL_PROP).as[String].leftMap(e =>
          SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))

        def sendDataStart(imageFileId: ImageFileId): SeqAction[Unit] = for {
          d <- dataId
          _ <- systems.odb.datasetStart(obsId, d, imageFileId)
        } yield ()
        def sendDataEnd(imageFileId: ImageFileId): SeqAction[Unit] = for {
          d <- dataId
          _ <- systems.odb.datasetComplete(obsId, d, imageFileId)
        } yield ()

        def closeImage(id: ImageFileId, client: DhsClient): SeqAction[Unit] =
          client.setKeywords(id, KeywordBag(StringKeyword("instrument", inst.dhsInstrumentName)), finalFlag = true)

        for {
          id <- systems.dhs.createImage(DhsClient.ImageParameters(DhsClient.Permanent, List(inst.contributorName, "dhs-http")))
          _ <- sendDataStart(id)
          _ <- headers.map(_.sendBefore(id, inst.dhsInstrumentName)).sequenceU
          _ <- inst.observe(config)(id)
          _ <- headers.map(_.sendAfter(id, inst.dhsInstrumentName)).sequenceU
          _ <- closeImage(id, systems.dhs)
          - <- sendDataEnd(id)
        } yield ObserveResult(id)
      }

      Step[Action](
        i,
        None,
        config.toStepConfig,
        false,
        List(
          sys.map(x => toAction(x.configure(config).map(y => Result.Configured(y.sys.name)))),
          List(toAction(observe(config).map(x => Result.Observed(x.dataId))))
        )
      )
    }

    val instName = extractInstrumentName(config)

    instName match {
      case Flamingos2.name => buildStep(Flamingos2(systems.flamingos2), List(
        Flamingos2Header(systems.dhs, new Flamingos2Header.ObsKeywordsReaderImpl(config),
          if(settings.f2Keywords) Flamingos2Header.InstKeywordReaderImpl else Flamingos2Header.DummyInstKeywordReader,
          if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader
        ))
      ).right
      case _               => UnrecognizedInstrument(instName.toString).left[Step[Action]]
    }

  }

  private def extractInstrumentName(config: Config): String =
    // This is too weak. We may want to use the extractors used in ITC
    config.getItemValue(new ItemKey(INSTRUMENT_KEY, INSTRUMENT_NAME_PROP)).toString

  def sequence(systems: Systems, settings: Settings)(obsId: SPObservationID, sequenceConfig: ConfigSequence): SeqexecFailure \/ Sequence[Action] = {
    val configs = sequenceConfig.getAllSteps.toList

    val steps = configs.zipWithIndex.traverseU {
      case (c, i) => step(systems, settings)(obsId, i, c)
    }

    val instName = configs.headOption.map(extractInstrumentName).getOrElse("Unknown instrument")

    steps.map(Sequence[Action](obsId.stringValue(), SequenceMetadata(instName, "", ""), _))
  }

}

object SeqTranslate {
  def apply(site: Site) = new SeqTranslate(site)

  case class Systems(
                      odb: ODBProxy,
                      dhs: DhsClient,
                      tcs: TcsController,
                      flamingos2: Flamingos2Controller
                    )

  case class Settings(
                       tcsKeywords: Boolean,
                       f2Keywords: Boolean
                     )

}
