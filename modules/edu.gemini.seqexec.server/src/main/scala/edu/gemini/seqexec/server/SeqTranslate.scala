// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.spModel.core.Site
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine.Result.{FileIdAllocated, Observed}
import edu.gemini.seqexec.engine.{Action, ActionMetadata, Event, Result, Sequence, Step, fromTask}
import edu.gemini.seqexec.model.Model.{Resource, SequenceMetadata, StepState}
import edu.gemini.seqexec.model.Model
import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.DhsClient.{KeywordBag, StringKeyword}
import edu.gemini.seqexec.server.SeqTranslate.{Settings, Systems}
import edu.gemini.seqexec.server.SeqexecFailure.{Unexpected, UnrecognizedInstrument}
import edu.gemini.seqexec.server.TcsController.ScienceFoldPosition
import edu.gemini.spModel.ao.AOConstants._
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.gemini.altair.AltairConstants
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.seqexec.odb.{ExecutedDataset, SeqexecSequence}
import edu.gemini.seqexec.server.InstrumentSystem.{AbortObserveCmd, Controllable, StopObserveCmd}

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process

/**
  * Created by jluhrs on 9/14/16.
  */
class SeqTranslate(site: Site, systems: Systems, settings: Settings) {

  import SeqTranslate._

  private def dhsFileId(inst: InstrumentSystem): SeqAction[ImageFileId] =
    systems.dhs.createImage(DhsClient.ImageParameters(DhsClient.Permanent, List(inst.contributorName, "dhs-http")))

  private def observe(config: Config, obsId: SPObservationID, inst: InstrumentSystem,
                      otherSys: List[System], headers: Reader[ActionMetadata,List[Header]])
                     (ctx: ActionMetadata): SeqAction[Result.Partial[FileIdAllocated]] = {
    val dataId: SeqAction[String] = EitherT(Task(
      config.extract(OBSERVE_KEY / DATA_LABEL_PROP).as[String].leftMap(e =>
      SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))

    def sendDataStart(imageFileId: ImageFileId): SeqAction[Unit] = for {
      d <- dataId
      _ <- systems.odb.datasetStart(obsId, d, imageFileId)
    } yield ()

    def sendDataEnd(imageFileId: ImageFileId): SeqAction[Unit] = for {
      d <- dataId
      _ <- systems.odb.datasetComplete(obsId, d, imageFileId)
    } yield ()

    def notifyObserveStart: SeqAction[Unit] = otherSys.map(_.notifyObserveStart).sequenceU.map(_ => ())

    def notifyObserveEnd: SeqAction[Unit] = otherSys.map(_.notifyObserveEnd).sequenceU.map(_ => ())

    def closeImage(id: ImageFileId, client: DhsClient): SeqAction[Unit] =
      client.setKeywords(id, KeywordBag(StringKeyword("instrument", inst.dhsInstrumentName)), finalFlag = true)

    def doObserve(id: ImageFileId): SeqAction[ObserveResult] =
      for {
        _  <- sendDataStart(id)
        _  <- notifyObserveStart
        _  <- headers(ctx).map(_.sendBefore(id, inst.dhsInstrumentName)).sequenceU
        _  <- inst.observe(config)(id)
        _  <- notifyObserveEnd
        _  <- headers(ctx).reverseMap(_.sendAfter(id, inst.dhsInstrumentName)).sequenceU
        _  <- closeImage(id, systems.dhs)
        _  <- sendDataEnd(id)
      } yield ObserveResult(id)

    for {
      id <- dhsFileId(inst)
    } yield Result.Partial(FileIdAllocated(id), doObserve(id).toAction)
  }

  private def step(obsId: SPObservationID, i: Int, config: Config, last: Boolean, datasets: Map[Int, ExecutedDataset]): TrySeq[Step[Action \/ Result]] = {
    def buildStep(inst: InstrumentSystem, sys: List[System], headers: Reader[ActionMetadata,List[Header]], resources: Set[Resource]): Step[Action \/ Result] = {
      val initialStepExecutions: List[List[Action]] =
        if (i === 0) List(List(systems.odb.sequenceStart(obsId, "").map(_ => Result.Ignored).toAction))
        else Nil

      val lastStepExecutions: List[List[Action]] =
        if (last) List(List(systems.odb.sequenceEnd(obsId).map(_ => Result.Ignored).toAction))
        else Nil

      val regularStepExecutions: List[List[Action]] =
        List(
          sys.map(x => x.configure(config).map(y => Result.Configured(y.sys.name)).toAction),
          List(new Action(ctx => observe(config, obsId, inst, sys.filterNot(inst.equals), headers)(ctx).run.map(_.toResult))))

      extractStatus(config) match {
        case StepState.Pending =>
          Step(
            id = i,
            fileId = None,
            config = config.toStepConfig,
            resources = resources,
            breakpoint = false,
            skip = false,
            executions = initialStepExecutions ++ regularStepExecutions ++ lastStepExecutions
          ).map(_.left)
        // TODO: This case should be for completed Steps only. Fail when step
        // status is unknown.
        case _ =>
          Step(
            i,
            datasets.get(i + 1).map(_.filename), // Note that steps on datasets are indexed starting on 1
            config.toStepConfig,
            // No resources when done
            resources = Set.empty,
            breakpoint = false,
            skip = extractSkipped(config),
            // TODO: Is it possible to reconstruct done executions from the ODB?
            executions = Nil
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

  // Required for untyped objects from java
  implicit val objectShow: Show[AnyRef] = Show.showA

  private def extractInstrumentName(config: Config): SeqexecFailure \/ edu.gemini.seqexec.model.Model.Instrument =
    // This is too weak. We may want to use the extractors used in ITC
    config.getItemValue(new ItemKey(INSTRUMENT_KEY, INSTRUMENT_NAME_PROP)) match {
      case "Flamingos2" => edu.gemini.seqexec.model.Model.Instrument.F2.right
      case "GMOS-S"     => edu.gemini.seqexec.model.Model.Instrument.GmosS.right
      case "GMOS-N"     => edu.gemini.seqexec.model.Model.Instrument.GmosN.right
      case n            => SeqexecFailure.UnrecognizedInstrument(s"$n").left
    }

  private def extractStatus(config: Config): StepState =
    config.getItemValue(new ItemKey("observe:status")).shows match {
      case "ready"    => StepState.Pending
      case "complete" => StepState.Completed
      case kw         => StepState.Error("Unexpected status keyword: " ++ kw)
    }

  private def extractSkipped(config: Config): Boolean =
    config.getItemValue(new ItemKey("observe:status")).shows match {
      case "skipped" => true
      case _         => false
    }

  def sequence(obsId: SPObservationID, sequence: SeqexecSequence):
      (List[SeqexecFailure], Option[Sequence[Action \/ Result]]) = {

    val configs = sequence.config.getAllSteps.toList

    val steps = configs.zipWithIndex.map {
      case (c, i) => step(obsId, i, c, i === (configs.length - 1), sequence.datasets)
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
                SequenceMetadata(i, None, sequence.title),
                ss
              )
            )
        )
      })
  }

  def stopObserve(inst: Model.Instrument): Option[Process[Task, Event]] =
    toInstrumentSys(inst).toOption.flatMap(_.observeControl match {
      case Controllable(StopObserveCmd(stop), _, _, _) => Some(Process.eval(stop.run.map{
        case -\/(e) => Event.logMsg(SeqexecFailure.explain(e))
        case _      => Event.nullEvent
      }))
      case _                                           => none
    } )

  def abortObserve(inst: Model.Instrument): Option[Process[Task, Event]] =
    toInstrumentSys(inst).toOption.flatMap(_.observeControl match {
      case Controllable(_, AbortObserveCmd(abort), _, _) => Some(Process.eval(abort.run.map{
        case -\/(e) => Event.logMsg(SeqexecFailure.explain(e))
        case _      => Event.nullEvent
      }))
      case _                                           => none
    } )

  private def toInstrumentSys(inst: Model.Instrument): TrySeq[InstrumentSystem] = inst match {
    case Model.Instrument.F2    => TrySeq(Flamingos2(systems.flamingos2))
    case Model.Instrument.GmosS => TrySeq(GmosSouth(systems.gmosSouth))
    case Model.Instrument.GmosN => TrySeq(GmosNorth(systems.gmosNorth))
    case _                      => TrySeq.fail(Unexpected(s"Instrument $inst not supported."))
  }

  private def calcResources(stepType: StepType): TrySeq[Set[Resource]] = stepType match {
    case CelestialObject(inst) => TrySeq(Set(inst, Resource.TCS, Resource.Gcal))
    case FlatOrArc(inst)       => TrySeq(Set(inst, Resource.Gcal, Resource.TCS))
    case DarkOrBias(inst)      => TrySeq(Set(inst))
    case _                     => TrySeq.fail(Unexpected(s"Unsupported step type $stepType"))
  }

  import TcsController.Subsystem._

  private def hasOI(inst: Model.Instrument): Boolean = inst match {
    case Model.Instrument.F2    => true
    case Model.Instrument.GmosS => true
    case Model.Instrument.GmosN => true
    case Model.Instrument.NIFS  => true
    case Model.Instrument.NIRI  => true
    case _                      => false
  }

  private def calcSystems(stepType: StepType): TrySeq[List[System]] = {
    stepType match {
      case CelestialObject(inst) => toInstrumentSys(inst).map(_ :: List(Tcs(systems.tcs, all, ScienceFoldPosition.Position(TcsController.LightSource.Sky, inst)), Gcal(systems.gcal, site == Site.GS)))
      case FlatOrArc(inst)       => toInstrumentSys(inst).map(_ :: List(Tcs(systems.tcs, NonEmptyList[TcsController.Subsystem](ScienceFold) :::> (if(hasOI(inst)) IList(OIWFS) else IList.empty), ScienceFoldPosition.Position(TcsController.LightSource.GCAL, inst)), Gcal(systems.gcal, site == Site.GS)))
      case DarkOrBias(inst)      => toInstrumentSys(inst).map(List(_))
      case _                     => TrySeq.fail(Unexpected(s"Unsupported step type $stepType"))
    }
  }

  private def calcInstHeader(config: Config, inst: Model.Instrument): TrySeq[Header] = inst match {
    case Model.Instrument.F2     =>  TrySeq(Flamingos2Header(systems.dhs, new Flamingos2Header.ObsKeywordsReaderImpl(config),
      if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader))
    case Model.Instrument.GmosS |
         Model.Instrument.GmosN  =>
      val tcsReader: TcsKeywordsReader = if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader
      val gmosInstReader = if (settings.gmosKeywords) GmosHeader.InstKeywordReaderImpl else GmosHeader.DummyInstKeywordReader
      TrySeq(GmosHeader(systems.dhs, GmosHeader.ObsKeywordsReaderImpl(config), gmosInstReader, tcsReader))
    case _                       =>  TrySeq.fail(Unexpected(s"Instrument $inst not supported."))
  }

  private def commonHeaders(config: Config)(ctx: ActionMetadata): Header = new StandardHeader(systems.dhs,
    ObsKeywordReaderImpl(config, site.displayName.replace(' ', '-')),
    if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader,
    StateKeywordsReader(ctx.conditions, ctx.operator, ctx.observer)
  )

  private val gwsHeaders: Header = new GwsHeader(systems.dhs,
    if (settings.gwsKeywords) GwsKeywordsReaderImpl else DummyGwsKeywordsReader
  )

  private val gcalHeader: Header = new GcalHeader(
    systems.dhs,
    if (settings.gcalKeywords) GcalKeywordsReaderImpl else DummyGcalKeywordsReader)

  private def calcHeaders(config: Config, stepType: StepType): TrySeq[Reader[ActionMetadata, List[Header]]] = stepType match {
    case CelestialObject(inst) => calcInstHeader(config, inst).map(h => Reader(ctx => List(commonHeaders(config)(ctx), gwsHeaders, h)))
    case FlatOrArc(inst)       => calcInstHeader(config, inst).map(h => Reader(ctx => List(commonHeaders(config)(ctx), gcalHeader, gwsHeaders, h)))
    case DarkOrBias(inst)      => calcInstHeader(config, inst).map(h => Reader(ctx => List(commonHeaders(config)(ctx), gwsHeaders, h)))
    case st                    => TrySeq.fail(Unexpected(s"Unsupported step type $st"))
  }

}

object SeqTranslate {
  def apply(site: Site, systems: Systems, settings: Settings): SeqTranslate = new SeqTranslate(site, systems, settings)

  final case class Systems(
                      odb: ODBProxy,
                      dhs: DhsClient,
                      tcs: TcsController,
                      gcal: GcalController,
                      flamingos2: Flamingos2Controller,
                      gmosSouth: GmosController.GmosSouthController,
                      gmosNorth: GmosController.GmosNorthController
                    )

  final case class Settings(
                      tcsKeywords: Boolean,
                      f2Keywords: Boolean,
                      gwsKeywords: Boolean,
                      gcalKeywords: Boolean,
                      gmosKeywords: Boolean
                     )


  private sealed trait StepType {
    val instrument: Model.Instrument
  }

  private def extractInstrument(config: Config): TrySeq[Model.Instrument] = {
    config.extract(INSTRUMENT_KEY / INSTRUMENT_NAME_PROP).as[String].asTrySeq.flatMap {
      case Flamingos2.name => TrySeq(Model.Instrument.F2)
      case GmosSouth.name  => TrySeq(Model.Instrument.GmosS)
      case GmosNorth.name  => TrySeq(Model.Instrument.GmosN)
      case ins             => TrySeq.fail(UnrecognizedInstrument(ins))
    }
  }

  private final case class CelestialObject(override val instrument: Model.Instrument) extends StepType
  private final case class Dark(override val instrument: Model.Instrument) extends StepType
  private final case class NodAndShuffle(override val instrument: Model.Instrument) extends StepType
  private final case class Gems(override val instrument: Model.Instrument) extends StepType
  private final case class Altair(override val instrument: Model.Instrument) extends StepType
  private final case class FlatOrArc(override val instrument: Model.Instrument) extends StepType
  private final case class DarkOrBias(override val instrument: Model.Instrument) extends StepType
  private case object AlignAndCalib extends StepType {
    override val instrument = Model.Instrument.GPI
  }

  private def calcStepType(config: Config): TrySeq[StepType] = {
    def extractGaos(inst: Model.Instrument): TrySeq[StepType] = config.extract(new ItemKey(AO_CONFIG_NAME) / AO_SYSTEM_PROP).as[String] match {
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

  implicit class ResponseToResult(val r: SeqexecFailure \/ Result.Response) extends AnyVal {
    def toResult: Result = r match {
      case \/-(r) => Result.OK(r)
      case -\/(e) => Result.Error(SeqexecFailure.explain(e))
    }
  }

  implicit class PartialResultToResult[A <: Result.PartialVal](val r: SeqexecFailure \/ Result.Partial[A]) extends AnyVal {
    def toResult: Result = r match {
      case \/-(r) => r
      case -\/(e) => Result.Error(SeqexecFailure.explain(e))
    }
  }

  implicit class ObserveResultToResult[A <: Result.PartialVal](val r: SeqexecFailure \/ ObserveResult) extends AnyVal {
    def toResult: Result = r match {
      case \/-(r) => Result.OK(Observed(r.dataId))
      case -\/(e) => Result.Error(SeqexecFailure.explain(e))
    }
  }

  implicit class ActionResponseToAction[A <: Result.Response](val x: SeqAction[A]) extends AnyVal {
    def toAction: Action = fromTask(x.run.map(_.toResult))
  }

  implicit class ObserveResultToAction(val x: SeqAction[ObserveResult]) extends AnyVal {
    def toAction: Action = fromTask(x.run.map(_.toResult))
  }
}
