package edu.gemini.seqexec.server

import edu.gemini.model.p1.immutable.Site
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine.{Action, Resource, Result, Sequence, Step}
import edu.gemini.seqexec.model.Model.SequenceMetadata
import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.DhsClient.{KeywordBag, StringKeyword}
import edu.gemini.seqexec.server.SeqTranslate.{Settings, Systems}
import edu.gemini.seqexec.server.SeqexecFailure.{Unexpected, UnrecognizedInstrument}
import edu.gemini.spModel.ao.AOConstants._
import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
import edu.gemini.spModel.gemini.altair.AltairConstants
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

/**
  * Created by jluhrs on 9/14/16.
  */
class SeqTranslate(site: Site, systems: Systems, settings: Settings) {

  import SeqTranslate._

  implicit def toAction(x: SeqAction[Result.Response]): Action = x.run map {
    case -\/(e) => Result.Error(SeqexecFailure.explain(e))
    case \/-(r) => Result.OK(r)
  }

  private def step(obsId: SPObservationID, i: Int, config: Config, last: Boolean): TrySeq[Step[Action]] = {

    def buildStep(inst: Instrument, sys: List[System], headers: List[Header], resources: Set[Resource]): Step[Action] = {

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
          _ <- sendDataEnd(id)
        } yield ObserveResult(id)
      }

      Step[Action](
        i,
        None,
        config.toStepConfig,
        resources,
        false,
        (if(i == 0) List(List(toAction(systems.odb.sequenceStart(obsId, "").map(_ => Result.Ignored))))
        else List())
        ++
        List(
          sys.map(x => toAction(x.configure(config).map(y => Result.Configured(y.sys.name)))),
          List(toAction(observe(config).map(x => Result.Observed(x.dataId))))
        )
        ++
        (if(last) List(List(toAction(systems.odb.sequenceEnd(obsId).map(_ => Result.Ignored))))
        else List())
      )
    }


    for {
      stepType <- calcStepType(config)
      inst     <- toInstrumentSys(stepType.instrument)
      systems  <- calcSystems(stepType)
      headers  <- calcHeaders(config, stepType)
      resources <- calcResources(stepType)
    } yield buildStep(inst, systems, headers, resources)

  }

  private def extractInstrumentName(config: Config): String =
    // This is too weak. We may want to use the extractors used in ITC
    config.getItemValue(new ItemKey(INSTRUMENT_KEY, INSTRUMENT_NAME_PROP)).toString

  def sequence(settings: Settings)(obsId: SPObservationID, sequenceConfig: ConfigSequence): SeqexecFailure \/ Sequence[Action] = {
    val configs = sequenceConfig.getAllSteps.toList

    val steps = configs.zipWithIndex.traverseU {
      case (c, i) => step(obsId, i, c, i == (configs.length-1))
    }

    val instName = configs.headOption.map(extractInstrumentName).getOrElse("Unknown instrument")

    steps.map(Sequence[Action](obsId.stringValue(), SequenceMetadata(instName, None, None), _))
  }

  private def toInstrumentSys(inst: Resource.Instrument): TrySeq[Instrument] = inst match {
    case Resource.F2 => TrySeq(Flamingos2(systems.flamingos2))
    case _           => TrySeq.fail(Unexpected(s"Instrument ${inst.toString} not supported."))
  }

  private def calcResources(stepType: StepType): TrySeq[Set[Resource]] = stepType match {
    case CelestialObject(inst) => TrySeq(Set(inst, Resource.Mount, Resource.ScienceFold, Resource.Gcal))
    case FlatOrArc(inst)       => TrySeq(Set(inst, Resource.Gcal, Resource.ScienceFold))
    case _                     => TrySeq.fail(Unexpected(s"Unsupported step type ${stepType.toString}"))
  }

  private def calcSystems(stepType: StepType): TrySeq[List[System]] = {
    stepType match {
      case CelestialObject(inst) => toInstrumentSys(inst).map(_ :: List(Tcs(systems.tcs), Gcal(systems.gcal, site == Site.GS)))
      case FlatOrArc(inst)       => toInstrumentSys(inst).map(_ :: List(Gcal(systems.gcal, site == Site.GS)))
      case _ => TrySeq.fail(Unexpected(s"Unsupported step type ${stepType.toString}"))
    }
  }

  private def calcInstHeader(config: Config, inst: Resource.Instrument): TrySeq[Header] = inst match {
    case Resource.F2 =>  TrySeq(Flamingos2Header(systems.dhs, new Flamingos2Header.ObsKeywordsReaderImpl(config),
      if(settings.f2Keywords) Flamingos2Header.InstKeywordReaderImpl else Flamingos2Header.DummyInstKeywordReader,
      if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader))
    case _           =>  TrySeq.fail(Unexpected(s"Instrument ${inst.toString} not supported."))
  }

  private def commonHeaders(config: Config): List[Header] = List(new StandardHeader(systems.dhs,
    ObsKeywordReaderImpl(config, site.name.replace(' ', '-')),
    if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader,
    if (settings.gwsKeywords) GwsKeywordsReaderImpl else DummyGwsKeywordsReader,
    // TODO: Replace Unit by something that can read the State
    StateKeywordsReader(Unit)
  ))


  private def calcHeaders(config: Config, stepType: StepType): TrySeq[List[Header]] = stepType match {
    case CelestialObject(inst) => calcInstHeader(config, inst).map(_ :: commonHeaders(config))
    case FlatOrArc(inst)       => calcInstHeader(config, inst).map(_ :: commonHeaders(config)) //TODO: Add GCAL keywords here
    case DarkOrBias(inst)      => calcInstHeader(config, inst).map(_ :: commonHeaders(config))
    case st@_                  => TrySeq.fail(Unexpected("Unsupported step type " + st.toString))
  }

}

object SeqTranslate {
  def apply(site: Site, systems: Systems, settings: Settings) = new SeqTranslate(site, systems, settings)

  case class Systems(
                      odb: ODBProxy,
                      dhs: DhsClient,
                      tcs: TcsController,
                      gcal: GcalController,
                      flamingos2: Flamingos2Controller
                    )

  case class Settings(
                      tcsKeywords: Boolean,
                      f2Keywords: Boolean,
                      gwsKeywords: Boolean
                     )


  private sealed trait StepType {
    val instrument: Resource.Instrument
  }

  private final case class CelestialObject(override val instrument: Resource.Instrument) extends StepType
  private final case class Dark(override val instrument: Resource.Instrument) extends StepType
  private final case class NodAndShuffle(override val instrument: Resource.Instrument) extends StepType
  private final case class Gems(override val instrument: Resource.Instrument) extends StepType
  private final case class Altair(override val instrument: Resource.Instrument) extends StepType
  private final case class FlatOrArc(override val instrument: Resource.Instrument) extends StepType
  private final case class DarkOrBias(override val instrument: Resource.Instrument) extends StepType
  private case object AlignAndCalib extends StepType {
    override val instrument = Resource.GPI
  }



  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  private def extractInstrument(config: Config): TrySeq[Resource.Instrument] = {
    config.extract(INSTRUMENT_KEY / INSTRUMENT_NAME_PROP).as[String].leftMap(explainExtractError).flatMap{
      case Flamingos2.name => TrySeq(Resource.F2)
      case ins@_           => TrySeq.fail(UnrecognizedInstrument(ins))
    }
  }

  private def calcStepType(config: Config): TrySeq[StepType] = {
    def extractGaos(inst: Resource.Instrument): TrySeq[StepType] = config.extract(new ItemKey(AO_CONFIG_NAME) / AO_SYSTEM_PROP).as[String] match {
      case -\/(ConfigUtilOps.ConversionError(_,_)) => TrySeq.fail(Unexpected("Unable to get AO system from sequence"))
      case -\/(ConfigUtilOps.KeyNotFound(_))       => TrySeq(CelestialObject(inst))
      case \/-(gaos)                               => gaos match {
        case AltairConstants.SYSTEM_NAME_PROP                => TrySeq(Altair(inst))
        case edu.gemini.spModel.gemini.gems.Gems.SYSTEM_NAME => TrySeq(Gems(inst))
      }
    }


    ( config.extract(OBSERVE_KEY / OBSERVE_TYPE_PROP).as[String].leftMap(explainExtractError)
      |@| extractInstrument(config) ) { (obsType, inst) =>
      obsType match {
        case SCIENCE_OBSERVE_TYPE                     => extractGaos(inst)
        case BIAS_OBSERVE_TYPE | DARK_OBSERVE_TYPE    => TrySeq(DarkOrBias(inst))
        case FLAT_OBSERVE_TYPE | ARC_OBSERVE_TYPE | CAL_OBSERVE_TYPE
                                                      => TrySeq(FlatOrArc(inst))
        case _                                        => TrySeq.fail(Unexpected("Unknown step type " + obsType))
      }
    }.join
  }

}
