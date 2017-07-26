package edu.gemini.seqexec.server

import edu.gemini.spModel.core.Site
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine.{Action, Resource, Result, Sequence, Step, fromTask}
import edu.gemini.seqexec.model.Model.{SequenceMetadata, StepState}
import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.DhsClient.{KeywordBag, StringKeyword}
import edu.gemini.seqexec.server.SeqTranslate.{Settings, Systems}
import edu.gemini.seqexec.server.SeqexecFailure.{Unexpected, UnrecognizedInstrument}
import edu.gemini.seqexec.server.TcsController.ScienceFoldPosition
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

  def toResult(r: SeqexecFailure\/Result.Response): Result = r match {
    case \/-(r) => Result.OK(r)
    case -\/(e) => Result.Error(SeqexecFailure.explain(e))
  }

  def toAction(x: SeqAction[Result.Response]): Action = fromTask(x.run.map(toResult))

  private def step(
    obsId: SPObservationID,
    i: Int,
    config: Config,
    last: Boolean
  ): TrySeq[Step[Action \/ Result]] = {

    def buildStep(inst: Instrument, sys: List[System], headers: List[Header], resources: Set[Resource]): Step[Action \/ Result] = {

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

      extractStatus(config) match {
        case StepState.Pending =>
          Step(
            i,
            None,
            config.toStepConfig,
            resources,
            false,
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
          ).map(_.left)
        // TODO: This case should be for completed Steps only. Fail when step
        // status is unknown.
        case _ =>
          Step(
            i,
            // TODO: Get image fileId?
            None,
            config.toStepConfig,
            // No resources when done
            Set.empty,
            false,
            extractSkipped(config),
            // TODO: Is it possible to reconstruct done executions from the ODB?
            List(Nil)
            )
      }
    }

    for {
      stepType  <- calcStepType(config)
      inst      <- toInstrumentSys(stepType.instrument)
      systems   <- calcSystems(stepType)
      headers   <- calcHeaders(config, stepType)
      resources <- calcResources(stepType)
    } yield buildStep(inst, systems, headers, resources)

  }

  private def extractInstrumentName(config: Config): SeqexecFailure \/ edu.gemini.seqexec.model.Model.Instrument =
    // This is too weak. We may want to use the extractors used in ITC
    config.getItemValue(new ItemKey(INSTRUMENT_KEY, INSTRUMENT_NAME_PROP)) match {
      case "Flamingos2" => edu.gemini.seqexec.model.Model.F2.right
      case "GMOS-S"     => edu.gemini.seqexec.model.Model.GmosS.right
      case n            => SeqexecFailure.UnrecognizedInstrument(n.toString).left

    }

  private def extractStatus(config: Config): StepState =
    config.getItemValue(new ItemKey("observe:status")).toString match {
      case "ready"    => StepState.Pending
      case "complete" => StepState.Completed
      case kw         => StepState.Error("Unexpected status keyword: " ++ kw)
    }

  private def extractSkipped(config: Config): Boolean =
    config.getItemValue(new ItemKey("observe:status")).toString match {
      case "skipped" => true
      case _         => false
    }

  def sequence(settings: Settings)
              (obsId: SPObservationID, sequenceConfig: ConfigSequence, name: String):
      (List[SeqexecFailure], Option[Sequence[Action \/ Result]]) = {

    val configs = sequenceConfig.getAllSteps.toList

    val steps = configs.zipWithIndex.map {
      case (c, i) => step(obsId, i, c, i == (configs.length - 1))
    }.separate

    val instName = configs.headOption.map(extractInstrumentName).getOrElse(SeqexecFailure.UnrecognizedInstrument("UNKNOWN").left)

    instName.fold(e => (List(e), none), i =>
      steps match {
        case (errs, ss) => (
          errs,
          if (ss.isEmpty)
            None
          else
            Some(
              Sequence[Action \/ Result](
                obsId.stringValue(),
                SequenceMetadata(i, None, name),
                ss
              )
            )
        )
      })
  }

  private def toInstrumentSys(inst: Resource.Instrument): TrySeq[Instrument] = inst match {
    case Resource.F2   => TrySeq(Flamingos2(systems.flamingos2))
    case Resource.GMOS => TrySeq(GmosSouth(systems.gmosSouth))
    case _             => TrySeq.fail(Unexpected(s"Instrument ${inst.toString} not supported."))
  }

  private def calcResources(stepType: StepType): TrySeq[Set[Resource]] = stepType match {
    case CelestialObject(inst) => TrySeq(Set(inst, Resource.TCS, Resource.Gcal))
    case FlatOrArc(inst)       => TrySeq(Set(inst, Resource.Gcal, Resource.TCS))
    case DarkOrBias(inst)      => TrySeq(Set(inst))
    case _                     => TrySeq.fail(Unexpected(s"Unsupported step type ${stepType.toString}"))
  }

  import TcsController.Subsystem._

  private def hasOI(inst: Resource.Instrument): Boolean = inst match {
    case Resource.F2   => true
    case Resource.GMOS => true
    case Resource.NIFS => true
    case Resource.NIRI => true
    case _             => false
  }

  private def calcSystems(stepType: StepType): TrySeq[List[System]] = {
    stepType match {
      case CelestialObject(inst) => toInstrumentSys(inst).map(_ :: List(Tcs(systems.tcs, all, ScienceFoldPosition.Position(TcsController.LightSource.Sky, inst)), Gcal(systems.gcal, site == Site.GS)))
      case FlatOrArc(inst)       => toInstrumentSys(inst).map(_ :: List(Tcs(systems.tcs, NonEmptyList[TcsController.Subsystem](ScienceFold) :::> (if(hasOI(inst)) IList(OIWFS) else IList.empty), ScienceFoldPosition.Position(TcsController.LightSource.GCAL, inst)), Gcal(systems.gcal, site == Site.GS)))
      case DarkOrBias(inst)      => toInstrumentSys(inst).map(List(_))
      case _                     => TrySeq.fail(Unexpected(s"Unsupported step type ${stepType.toString}"))
    }
  }

  private def calcInstHeader(config: Config, inst: Resource.Instrument): TrySeq[Header] = inst match {
    case Resource.F2   =>  TrySeq(Flamingos2Header(systems.dhs, new Flamingos2Header.ObsKeywordsReaderImpl(config),
      if (settings.f2Keywords) Flamingos2Header.InstKeywordReaderImpl else Flamingos2Header.DummyInstKeywordReader,
      if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader))
    case Resource.GMOS =>
      val tcsReader: TcsKeywordsReader = if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader
      val gmosInstReader = if (settings.gmosKeywords) GmosHeader.InstKeywordReaderImpl else GmosHeader.DummyInstKeywordReader
      TrySeq(GmosHeader(systems.dhs, GmosHeader.ObsKeywordsReaderImpl(config), gmosInstReader, tcsReader))
    case _             =>  TrySeq.fail(Unexpected(s"Instrument ${inst.toString} not supported."))
  }

  private def commonHeaders(config: Config): Header = new StandardHeader(systems.dhs,
    ObsKeywordReaderImpl(config, site.displayName.replace(' ', '-')),
    if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader,
    // TODO: Replace Unit by something that can read the State
    StateKeywordsReader(Unit)
  )

  private val gwsHeaders: Header = new GwsHeader(systems.dhs,
    if (settings.gwsKeywords) GwsKeywordsReaderImpl else DummyGwsKeywordsReader
  )

  private val gcalHeader: Header = new GcalHeader(
    systems.dhs,
    if (settings.gcalKeywords) GcalKeywordsReaderImpl else DummyGcalKeywordsReader)

  private def calcHeaders(config: Config, stepType: StepType): TrySeq[List[Header]] = stepType match {
    case CelestialObject(inst) => calcInstHeader(config, inst).map(List(commonHeaders(config), gwsHeaders, _))
    case FlatOrArc(inst)       => calcInstHeader(config, inst).map(List(commonHeaders(config), gcalHeader, gwsHeaders, _))
    case DarkOrBias(inst)      => calcInstHeader(config, inst).map(List(commonHeaders(config), gwsHeaders, _))
    case st                    => TrySeq.fail(Unexpected("Unsupported step type " + st.toString))
  }

}

object SeqTranslate {
  def apply(site: Site, systems: Systems, settings: Settings) = new SeqTranslate(site, systems, settings)

  case class Systems(
                      odb: ODBProxy,
                      dhs: DhsClient,
                      tcs: TcsController,
                      gcal: GcalController,
                      flamingos2: Flamingos2Controller,
                      gmosSouth: GmosSouthController
                    )

  case class Settings(
                      tcsKeywords: Boolean,
                      f2Keywords: Boolean,
                      gwsKeywords: Boolean,
                      gcalKeywords: Boolean,
                      gmosKeywords: Boolean
                     )


  private sealed trait StepType {
    val instrument: Resource.Instrument
  }

  private def extractInstrument(config: Config): TrySeq[Resource.Instrument] = {
    config.extract(INSTRUMENT_KEY / INSTRUMENT_NAME_PROP).as[String].asTrySeq.flatMap {
      case Flamingos2.name => TrySeq(Resource.F2)
      case GmosSouth.name  => TrySeq(Resource.GMOS)
      case ins             => TrySeq.fail(UnrecognizedInstrument(ins))
    }
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

  private def calcStepType(config: Config): TrySeq[StepType] = {
    def extractGaos(inst: Resource.Instrument): TrySeq[StepType] = config.extract(new ItemKey(AO_CONFIG_NAME) / AO_SYSTEM_PROP).as[String] match {
      case -\/(ConfigUtilOps.ConversionError(_, _)) => TrySeq.fail(Unexpected("Unable to get AO system from sequence"))
      case -\/(ConfigUtilOps.ContentError(_))       => TrySeq.fail(Unexpected("Logical error"))
      case -\/(ConfigUtilOps.KeyNotFound(_))        => TrySeq(CelestialObject(inst))
      case \/-(gaos)                                =>
        gaos match {
          case AltairConstants.SYSTEM_NAME_PROP                => TrySeq(Altair(inst))
          case edu.gemini.spModel.gemini.gems.Gems.SYSTEM_NAME => TrySeq(Gems(inst))
          case _                                               => TrySeq.fail(Unexpected("Logical error reading AO system name"))
        }
    }

    (config.extract(OBSERVE_KEY / OBSERVE_TYPE_PROP).as[String].leftMap(explainExtractError)
      |@| extractInstrument(config)) { (obsType, inst) =>
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
