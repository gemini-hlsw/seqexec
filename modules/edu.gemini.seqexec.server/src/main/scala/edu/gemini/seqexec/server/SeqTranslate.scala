// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.spModel.core.Site
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine.Result.{Configured, FileIdAllocated, Observed}
import edu.gemini.seqexec.engine.{Action, ActionMetadata, Event, Result, Sequence, Step, fromTask}
import edu.gemini.seqexec.model.Model.{Instrument, Resource, SequenceMetadata, StepState}
import edu.gemini.seqexec.model.{ActionType, Model}
import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.DhsClient.{KeywordBag, StringKeyword}
import edu.gemini.seqexec.server.SeqTranslate.{Settings, Systems}
import edu.gemini.seqexec.server.SeqexecFailure.{Unexpected, UnrecognizedInstrument}
import edu.gemini.seqexec.server.InstrumentSystem._
import edu.gemini.seqexec.server.flamingos2.{Flamingos2, Flamingos2Controller, Flamingos2Header}
import edu.gemini.seqexec.server.gcal._
import edu.gemini.seqexec.server.gmos.{GmosController, GmosHeader, GmosNorth, GmosSouth}
import edu.gemini.seqexec.server.gws.{DummyGwsKeywordsReader, GwsHeader, GwsKeywordsReaderImpl}
import edu.gemini.seqexec.server.tcs._
import edu.gemini.seqexec.server.tcs.TcsController.ScienceFoldPosition
import edu.gemini.seqexec.odb.{ExecutedDataset, SeqexecSequence}
import edu.gemini.spModel.ao.AOConstants._
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.gemini.altair.AltairConstants
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import squants.Time

/**
  * Created by jluhrs on 9/14/16.
  */
class SeqTranslate(site: Site, systems: Systems, settings: Settings) {

  import SeqTranslate._

  private def dhsFileId(inst: InstrumentSystem): SeqAction[ImageFileId] =
    systems.dhs.createImage(DhsClient.ImageParameters(DhsClient.Permanent, List(inst.contributorName, "dhs-http")))

  private def sendDataStart(obsId: SPObservationID, imageFileId: ImageFileId, dataId: String): SeqAction[Unit] =
    systems.odb.datasetStart(obsId, dataId, imageFileId).flatMap{
      if(_) SeqAction.void
      else SeqAction.fail(SeqexecFailure.Unexpected("Unable to send DataStart message to ODB."))
    }

  private def sendDataEnd(obsId: SPObservationID, imageFileId: ImageFileId, dataId: String): SeqAction[Unit] =
    systems.odb.datasetComplete(obsId, dataId, imageFileId).flatMap{
      if(_) SeqAction.void
      else SeqAction.fail(SeqexecFailure.Unexpected("Unable to send DataEnd message to ODB."))
    }

  private def sendObservationAborted(obsId: SPObservationID, imageFileId: ImageFileId): SeqAction[Unit] =
    systems.odb.obsAbort(obsId, imageFileId).flatMap{
      if(_) SeqAction.void
      else SeqAction.fail(SeqexecFailure.Unexpected("Unable to send ObservationAborted message to ODB."))
    }
  private def observe(config: Config, obsId: SPObservationID, inst: InstrumentSystem,
                      otherSys: List[System], headers: Reader[ActionMetadata,List[Header]])
                     (ctx: ActionMetadata): SeqAction[Result.Partial[FileIdAllocated]] = {
    val dataId: SeqAction[String] = EitherT(Task(
      config.extract(OBSERVE_KEY / DATA_LABEL_PROP).as[String].leftMap(e =>
      SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))

    def notifyObserveStart: SeqAction[Unit] = otherSys.map(_.notifyObserveStart).sequenceU.map(_ => ())

    // endObserve must be sent to the instrument too.
    def notifyObserveEnd: SeqAction[Unit] = (inst +: otherSys).map(_.notifyObserveEnd).sequenceU.map(_ => ())

    def closeImage(id: ImageFileId, client: DhsClient): SeqAction[Unit] =
      client.setKeywords(id, KeywordBag(StringKeyword("instrument", inst.dhsInstrumentName)), finalFlag = true)

    def doObserve(id: ImageFileId): SeqAction[Result] =
      for {
        d   <- dataId
        _   <- sendDataStart(obsId, id, d)
        _  <- notifyObserveStart
        _  <- headers(ctx).map(_.sendBefore(id, inst.dhsInstrumentName)).sequenceU
        r   <- inst.observe(config)(id)
        ret <- observeTail(id, d)(r)
      } yield ret

    def observeTail(id: ImageFileId, dataId: String)(r: ObserveCommand.Result): SeqAction[Result] = {
      val successTail: SeqAction[Result] = for {
        _ <- notifyObserveEnd
        _ <- headers(ctx).reverseMap(_.sendAfter(id, inst.dhsInstrumentName)).sequenceU
        _ <- closeImage(id, systems.dhs)
        _ <- sendDataEnd(obsId, id, dataId)
      } yield Result.OK(Observed(id))
      val stopTail: SeqAction[Result] = successTail
      val abortTail: SeqAction[Result] = sendObservationAborted(obsId, id) *>
        SeqAction.fail(SeqexecFailure.Execution(s"Observation $id aborted by user."))

      r match {
        case ObserveCommand.Success => successTail
        case ObserveCommand.Stopped => stopTail
        case ObserveCommand.Aborted => abortTail
        case ObserveCommand.Paused => SeqAction(Result.Paused(ObserveContext(observeTail(id, dataId), inst.calcObserveTime(config))))
      }
    }

    for {
      id <- dhsFileId(inst)
    } yield Result.Partial(FileIdAllocated(id), Kleisli(_ => doObserve(id).run.map(_.toResult)))
  }

  // Introduced class StepBuilder to avoid warning for an excessively long function.
  private final class StepBuilder(obsId: SPObservationID, i: Int, config: Config, last: Boolean, datasets: Map[Int, ExecutedDataset]) {
    private def buildStep(inst: InstrumentSystem, sys: List[System], headers: Reader[ActionMetadata, List[Header]], resources: Set[Resource]): Step = {
      val initialStepExecutions: List[List[Action]] =
        if (i === 0) List(List(systems.odb.sequenceStart(obsId, "").map(_ => Result.Ignored).toAction(ActionType.Undefined)))
        else Nil

      val lastStepExecutions: List[List[Action]] =
        if (last) List(List(systems.odb.sequenceEnd(obsId).map(_ => Result.Ignored).toAction(ActionType.Undefined)))
        else Nil

      val regularStepExecutions: List[List[Action]] =
        List(
          sys.map { x =>
            val kind = ActionType.Configure(resourceFromSystem(x))
            x.configure(config).map(_ => Result.Configured(x.resource)).toAction(kind)
          },
          List(Action(ActionType.Observe, Kleisli(ctx => observe(config, obsId, inst, sys.filterNot(inst.equals), headers)(ctx).run.map(_.toResult)), Action.State(Action.Idle, Nil))))

      extractStatus(config) match {
        case StepState.Pending => Step.step(
          id = i,
          fileId = None,
          config = config.toStepConfig,
          resources = resources,
          executions = initialStepExecutions ++ regularStepExecutions ++ lastStepExecutions
        )
        // TODO: This case should be for completed Steps only. Fail when step
        // status is unknown.
        case _ => Step(
          id = i,
          fileId = datasets.get(i + 1).map(_.filename), // Note that steps on datasets are indexed starting on 1
          config = config.toStepConfig,
          // No resources when done
          resources = Set.empty,
          breakpoint = false,
          skipped = extractSkipped(config),
          skipMark = false,
          // TODO: Is it possible to reconstruct done executions from the ODB?
          executions = Nil
        )
      }
    }

    val step: TrySeq[Step] = {
      for {
        stepType <- calcStepType(config)
        inst <- toInstrumentSys(stepType.instrument)
        systems <- calcSystems(stepType)
        headers <- calcHeaders(config, stepType)
      } yield buildStep(inst, systems, headers, calcResources(systems))
    }
  }

  private object StepBuilder {
    def apply(obsId: SPObservationID, i: Int, config: Config, last: Boolean, datasets: Map[Int, ExecutedDataset]): StepBuilder =
      new StepBuilder(obsId, i, config, last, datasets)
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
      case kw         => StepState.Failed("Unexpected status keyword: " ++ kw)
    }

  private def extractSkipped(config: Config): Boolean =
    config.getItemValue(new ItemKey("observe:status")).shows match {
      case "skipped" => true
      case _         => false
    }

  def sequence(obsId: SPObservationID, sequence: SeqexecSequence):
      (List[SeqexecFailure], Option[Sequence]) = {

    val configs = sequence.config.getAllSteps.toList

    val steps = configs.zipWithIndex.map {
      case (c, i) => StepBuilder(obsId, i, c, i === (configs.length - 1), sequence.datasets).step
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
              Sequence(
                obsId.stringValue(),
                SequenceMetadata(i, None, sequence.title),
                ss
              )
            )
        )
      })
  }

  private def deliverObserveCmd(seqState: Sequence.State, cmd: Option[Process[Task, Event]]): Option[Process[Task, Event]] = {
    def isObserving(v: Action): Boolean = v.kind === ActionType.Observe && (v.state.runState match {
      case Action.Started               => true
      case _                            => false
    })
    if(seqState.current.execution.exists(isObserving)) cmd
    else none
  }

  def stopObserve(seqId: Sequence.Id)(seqState: Sequence.State): Option[Process[Task, Event]] = deliverObserveCmd(
    seqState,
    toInstrumentSys(seqState.toSequence.metadata.instrument).toOption.flatMap(_.observeControl match {
      case Controllable(StopObserveCmd(stop), _, _, _, _, _) => Some(Process.eval(stop.run.map{
        case -\/(e) => Event.logMsg(SeqexecFailure.explain(e))
        case _      => Event.nullEvent
      }))
      case _                                                 => none
    } )
  ).orElse(stopPaused(seqId)(seqState))

  def abortObserve(seqId: Sequence.Id)(seqState: Sequence.State): Option[Process[Task, Event]] = deliverObserveCmd(
    seqState,
    toInstrumentSys(seqState.toSequence.metadata.instrument).toOption.flatMap(_.observeControl match {
      case Controllable(_, AbortObserveCmd(abort), _, _, _, _) => Some(Process.eval(abort.run.map{
        case -\/(e) => Event.logMsg(SeqexecFailure.explain(e))
        case _      => Event.nullEvent
      }))
      case _                                                   => none
    } )
  ).orElse(abortPaused(seqId)(seqState))

  def pauseObserve(seqState: Sequence.State): Option[Process[Task, Event]] = deliverObserveCmd( seqState,
    toInstrumentSys(seqState.toSequence.metadata.instrument).toOption.flatMap(_.observeControl match {
      case Controllable(_, _, PauseObserveCmd(pause), _, _, _) => Some(Process.eval(pause.run.map{
        case -\/(e) => Event.logMsg(SeqexecFailure.explain(e))
        case _      => Event.nullEvent
      }))
      case _                                                   => none
    } )
  )

  private def pausedCommand(seqId: Sequence.Id, f: ObserveControl => Option[Time => SeqAction[ObserveCommand.Result]])
                           (seqState: Sequence.State): Option[Process[Task, Event]] = {
    val observeIndex: Option[(ObserveContext, Int)] =
      seqState.current.execution.zipWithIndex.find(_._1.kind === ActionType.Observe).flatMap{ case (a, i) =>
        a.state.runState match {
          case Action.Paused(c: ObserveContext) => Some((c, i))
          case _                                => none
        }
      }
    def resumeTask(c: ObserveContext, resumeCmd: SeqAction[ObserveCommand.Result]): Task[Result] = (for {
      r <- resumeCmd
      ret <- c.t(r)
    } yield ret ).run.map(_.toResult)

    observeIndex.flatMap{ case (c, i) => toInstrumentSys(seqState.toSequence.metadata.instrument).toOption.flatMap(
      x => f(x.observeControl).map(v => Process.eval(Task(Event.actionResume(seqId, i, resumeTask(c, v(c.expTime))))))
    ) }
  }

  def resumePaused(seqId: Sequence.Id)(seqState: Sequence.State): Option[Process[Task, Event]] = {
    def f(o: ObserveControl): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case Controllable(_, _, _, ContinuePausedCmd(a), _, _) => Some(a)
      case _                                                 => none
    }

    pausedCommand(seqId, f)(seqState)
  }

  private def stopPaused(seqId: Sequence.Id)(seqState: Sequence.State): Option[Process[Task, Event]] = {
    def f(o: ObserveControl): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case Controllable(_, _, _, _, StopPausedCmd(a), _) => Some(_ => a)
      case _                                             => none
    }

    pausedCommand(seqId, f)(seqState)
  }

  private def abortPaused(seqId: Sequence.Id)(seqState: Sequence.State): Option[Process[Task, Event]] = {
    def f(o: ObserveControl): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case Controllable(_, _, _, _, _, AbortPausedCmd(a)) => Some(_ => a)
      case _                                              => none
    }

    pausedCommand(seqId, f)(seqState)
  }

  private def toInstrumentSys(inst: Model.Instrument): TrySeq[InstrumentSystem] = inst match {
    case Model.Instrument.F2    => TrySeq(Flamingos2(systems.flamingos2))
    case Model.Instrument.GmosS => TrySeq(GmosSouth(systems.gmosSouth))
    case Model.Instrument.GmosN => TrySeq(GmosNorth(systems.gmosNorth))
    case _                      => TrySeq.fail(Unexpected(s"Instrument $inst not supported."))
  }

  private def calcResources(sys: List[System]): Set[Resource] = sys.map(resourceFromSystem).toSet

  import TcsController.Subsystem._

  private def hasOI(inst: Model.Instrument): Boolean = inst match {
    case Model.Instrument.F2    => true
    case Model.Instrument.GmosS => true
    case Model.Instrument.GmosN => true
    case Model.Instrument.NIFS  => true
    case Model.Instrument.NIRI  => true
    case _                      => false
  }

  private def flatOrArcTcsSubsystems(inst: Model.Instrument) = NonEmptyList[TcsController.Subsystem](ScienceFold) :::> (if(hasOI(inst)) IList(OIWFS) else IList.empty)

  private def calcSystems(stepType: StepType): TrySeq[List[System]] = {
    stepType match {
      case CelestialObject(inst) => toInstrumentSys(inst).map(_ :: List(Tcs(systems.tcs, all, ScienceFoldPosition.Position(TcsController.LightSource.Sky, inst)), Gcal(systems.gcal, site == Site.GS)))
      case FlatOrArc(inst)       => toInstrumentSys(inst).map(_ :: List(Tcs(systems.tcs, flatOrArcTcsSubsystems(inst), ScienceFoldPosition.Position(TcsController.LightSource.GCAL, inst)), Gcal(systems.gcal, site == Site.GS)))
      case DarkOrBias(inst)      => toInstrumentSys(inst).map(List(_))
      case _                     => TrySeq.fail(Unexpected(s"Unsupported step type $stepType"))
    }
  }

  // I cannot use a sealed trait as base, because I cannot have all systems in one source file (too big),
  // so either I use an unchecked notation, or add a default case that throws an exception.
  private def resourceFromSystem(s: System): Resource = (s: @unchecked) match {
    case Tcs(_, _, _)  => Resource.TCS
    case Gcal(_, _)    => Resource.Gcal
    case GmosNorth(_)  => Instrument.GmosN
    case GmosSouth(_)  => Instrument.GmosS
    case Flamingos2(_) => Instrument.F2
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

  private def commonHeaders(config: Config, tcsSubsystems: List[TcsController.Subsystem])(ctx: ActionMetadata): Header = new StandardHeader(
    systems.dhs,
    ObsKeywordReaderImpl(config, site.displayName.replace(' ', '-')),
    if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader,
    StateKeywordsReader(ctx.conditions, ctx.operator, ctx.observer),
    tcsSubsystems
  )

  private val gwsHeaders: Header = new GwsHeader(systems.dhs,
    if (settings.gwsKeywords) GwsKeywordsReaderImpl else DummyGwsKeywordsReader
  )

  private val gcalHeader: Header = new GcalHeader(systems.dhs,
    if (settings.gcalKeywords) GcalKeywordsReaderImpl else DummyGcalKeywordsReader
  )

  private def calcHeaders(config: Config, stepType: StepType): TrySeq[Reader[ActionMetadata, List[Header]]] = stepType match {
    case CelestialObject(inst) => calcInstHeader(config, inst).map(h => Reader(ctx =>
      List(commonHeaders(config, all.toList)(ctx), gwsHeaders, h)))
    case FlatOrArc(inst)       => calcInstHeader(config, inst).map(h => Reader(ctx =>
      List(commonHeaders(config, flatOrArcTcsSubsystems(inst).toList)(ctx), gcalHeader, gwsHeaders, h)))
    case DarkOrBias(inst)      => calcInstHeader(config, inst).map(h => Reader(ctx =>
      List(commonHeaders(config, Nil)(ctx), gwsHeaders, h)))
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
    override val instrument: Instrument = Model.Instrument.GPI
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

  implicit class ResultToResult(val r: SeqexecFailure \/ Result) extends AnyVal {
    def toResult: Result = r match {
      case \/-(r) => r
      case -\/(e) => Result.Error(SeqexecFailure.explain(e))
    }
  }

  implicit class ConfigResultToResult[A <: Result.PartialVal](val r: SeqexecFailure \/ ConfigResult) extends AnyVal {
    def toResult: Result = r match {
      case \/-(r) => Result.OK(Configured(r.sys.resource))
      case -\/(e) => Result.Error(SeqexecFailure.explain(e))
    }
  }

  implicit class ActionResponseToAction[A <: Result.Response](val x: SeqAction[A]) extends AnyVal {
    def toAction(kind: ActionType): Action = fromTask(kind, x.run.map(_.toResult))
  }

  implicit class ConfigResultToAction(val x: SeqAction[ConfigResult]) extends AnyVal {
    def toAction(kind: ActionType): Action = fromTask(kind, x.run.map(_.toResult))
  }

  final case class ObserveContext(t: ObserveCommand.Result => SeqAction[Result], expTime: Time) extends Result.PauseContext
}
