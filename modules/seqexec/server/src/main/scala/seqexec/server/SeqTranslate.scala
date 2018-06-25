// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import edu.gemini.spModel.core.Site
import seqexec.engine.Result.{Configured, FileIdAllocated, Observed}
import seqexec.engine.{Action, ActionMetadata, Event, Result, Sequence, Step, fromIO}
import seqexec.model.Model.{Instrument, Resource, SequenceMetadata, StepState}
import seqexec.model.{ActionType, Model}
import seqexec.model.dhs.ImageFileId
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords.DhsClient
import seqexec.server.keywords.DhsClient.{KeywordBag, StringKeyword}
import seqexec.server.SeqTranslate.{Settings, Systems}
import seqexec.server.SeqexecFailure.{Unexpected, UnrecognizedInstrument}
import seqexec.server.InstrumentSystem._
import seqexec.server.flamingos2.{Flamingos2, Flamingos2Controller, Flamingos2Header}
import seqexec.server.gpi.{GPI, GPIController, GPIHeader}
import seqexec.server.gcal._
import seqexec.server.gmos.{GmosController, GmosHeader, GmosNorth, GmosSouth}
import seqexec.server.gws.{DummyGwsKeywordsReader, GwsHeader, GwsKeywordsReaderImpl}
import seqexec.server.tcs._
import seqexec.server.tcs.TcsController.ScienceFoldPosition
import seqexec.server.gnirs._
import edu.gemini.spModel.ao.AOConstants._
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.gemini.altair.AltairConstants
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import org.log4s._
import cats._
import cats.data.{EitherT, Kleisli, NonEmptyList, Reader}
import cats.effect.IO
import cats.implicits._
import edu.gemini.seqexec.odb.{ExecutedDataset, SeqexecSequence}
import squants.Time
import fs2.Stream
import gem.Observation
import mouse.all._

class SeqTranslate(site: Site, systems: Systems, settings: Settings) {
  private val Log = getLogger

  implicit val show: Show[InstrumentSystem] = Show.show(_.resource.show)

  import SeqTranslate._

  private def dhsFileId(inst: InstrumentSystem): SeqAction[ImageFileId] =
    systems.dhs.createImage(DhsClient.ImageParameters(DhsClient.Permanent, List(inst.contributorName, "dhs-http")))

  private def sendDataStart(obsId: Observation.Id, imageFileId: ImageFileId, dataId: String): SeqAction[Unit] =
    systems.odb.datasetStart(obsId, dataId, imageFileId).flatMap{
      if(_) SeqAction.void
      else SeqAction.fail(SeqexecFailure.Unexpected("Unable to send DataStart message to ODB."))
    }

  private def sendDataEnd(obsId: Observation.Id, imageFileId: ImageFileId, dataId: String): SeqAction[Unit] =
    systems.odb.datasetComplete(obsId, dataId, imageFileId).flatMap{
      if(_) SeqAction.void
      else SeqAction.fail(SeqexecFailure.Unexpected("Unable to send DataEnd message to ODB."))
    }

  private def sendObservationAborted(obsId: Observation.Id, imageFileId: ImageFileId): SeqAction[Unit] =
    systems.odb.obsAbort(obsId, imageFileId).flatMap{
      if(_) SeqAction.void
      else SeqAction.fail(SeqexecFailure.Unexpected("Unable to send ObservationAborted message to ODB."))
    }

  private def info(msg: => String): SeqAction[Unit] = EitherT.right(IO.apply(Log.info(msg)))

  private def observe[A <: InstrumentSystem: Show](config: Config, obsId: Observation.Id, inst: A,
                      otherSys: List[System], headers: Reader[ActionMetadata, List[Header]])
                     (ctx: ActionMetadata): SeqAction[Result.Partial[FileIdAllocated]] = {
    val dataId: SeqAction[String] = EitherT(IO.apply(
      config.extract(OBSERVE_KEY / DATA_LABEL_PROP).as[String].leftMap(e =>
      SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))))

    def notifyObserveStart: SeqAction[Unit] = otherSys.map(_.notifyObserveStart).sequence.map(_ => ())

    // endObserve must be sent to the instrument too.
    def notifyObserveEnd: SeqAction[Unit] = (inst +: otherSys).map(_.notifyObserveEnd).sequence.map(_ => ())

    def closeImage(id: ImageFileId, client: DhsClient): SeqAction[Unit] =
      client.setKeywords(id, KeywordBag(StringKeyword("instrument", inst.dhsInstrumentName)), finalFlag = true)

    def doObserve(fileId: ImageFileId): SeqAction[Result] =
      for {
        d   <- dataId
        _   <- sendDataStart(obsId, fileId, d)
        _   <- notifyObserveStart
        _   <- headers(ctx).map(_.sendBefore(fileId, inst.dhsInstrumentName)).sequence
        _   <- info(s"Start ${inst.show} observation $obsId with label $fileId")
        r   <- inst.observe(config)(fileId)
        _   <- info(s"Completed ${inst.show} observation $obsId with label $fileId")
        ret <- observeTail(fileId, d)(r)
      } yield ret

    def observeTail(id: ImageFileId, dataId: String)(r: ObserveCommand.Result): SeqAction[Result] = {
      val successTail: SeqAction[Result] = for {
        _ <- notifyObserveEnd
        _ <- headers(ctx).reverseMap(_.sendAfter(id, inst.dhsInstrumentName)).sequence
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
    } yield Result.Partial(FileIdAllocated(id), Kleisli(_ => doObserve(id).value.map(_.toResult)))
  }

  //scalastyle:off
  private def step(obsId: Observation.Id, i: Int, config: Config, nextToRun: Int, datasets: Map[Int, ExecutedDataset]): TrySeq[Step] = {
    def buildStep(inst: InstrumentSystem, sys: List[System], headers: Reader[ActionMetadata,List[Header]], resources: Set[Resource]): Step = {
      val initialStepExecutions: List[List[Action]] =
        if (i === 0) List(List(systems.odb.sequenceStart(obsId, "").map(_ => Result.Ignored).toAction(ActionType.Undefined)))
        else Nil

      val regularStepExecutions: List[List[Action]] =
        List(
          sys.map { x =>
            val kind = ActionType.Configure(resourceFromSystem(x))
            x.configure(config).map(_ => Result.Configured(x.resource)).toAction(kind)
          },
          List(Action(ActionType.Observe, Kleisli(ctx => observe(config, obsId, inst, sys.filterNot(inst.equals), headers)(ctx).value.map(_.toResult)), Action.State(Action.Idle, Nil))))
      extractStatus(config) match {
        case StepState.Pending if i >= nextToRun => Step.init(
          id = i,
          fileId = None,
          config = config.toStepConfig,
          resources = resources,
          executions = initialStepExecutions ++ regularStepExecutions
        )
        case StepState.Pending => Step.init(
          id = i,
          fileId = datasets.get(i + 1).map(_.filename), // Note that steps on datasets are indexed starting on 1
          config = config.toStepConfig,
          // No resources when done
          resources = Set.empty,
          // TODO: Is it possible to reconstruct done executions from the ODB?
          executions = Nil
        ).copy(skipped = Step.Skipped(true))
        // TODO: This case should be for completed Steps only. Fail when step
        // status is unknown.
        case _ => Step.init(
          id = i,
          fileId = datasets.get(i + 1).map(_.filename), // Note that steps on datasets are indexed starting on 1
          config = config.toStepConfig,
          // No resources when done
          resources = Set.empty,
          // TODO: Is it possible to reconstruct done executions from the ODB?
          executions = Nil
        ).copy(skipped = Step.Skipped(extractSkipped(config)))
      }
    }

    for {
      stepType  <- calcStepType(config)
      inst      <- toInstrumentSys(stepType.instrument)
      systems   <- calcSystems(stepType)
      headers   <- calcHeaders(config, stepType)
    } yield buildStep(inst, systems, headers, calcResources(systems))
  }
  //scalastyle:on

  // Required for untyped objects from java
  implicit val objectShow: Show[AnyRef] = Show.fromToString

  private def extractStatus(config: Config): StepState =
    config.getItemValue(new ItemKey("observe:status")).show match {
      case "ready"    => StepState.Pending
      case "complete" => StepState.Completed
      case "skipped"  => StepState.Skipped
      case kw         => StepState.Failed("Unexpected status keyword: " ++ kw)
    }

  private def extractSkipped(config: Config): Boolean =
    config.getItemValue(new ItemKey("observe:status")).show match {
      case "skipped" => true
      case _         => false
    }

  def sequence(obsId: Observation.Id, sequence: SeqexecSequence):
      (List[SeqexecFailure], Option[Sequence]) = {

    val configs = sequence.config.getAllSteps.toList

    val nextToRun = configs.map(extractStatus).lastIndexWhere(s => s === StepState.Completed || s === StepState.Skipped) + 1

    val steps = configs.zipWithIndex.map {
      case (c, i) => step(obsId, i, c, nextToRun, sequence.datasets)
    }.separate

    val instName = configs.headOption.map(extractInstrument).getOrElse(Either.left(SeqexecFailure.UnrecognizedInstrument("UNKNOWN")))

    instName.fold(e => (List(e), none), i =>
      steps match {
        case (errs, ss) => (
          errs,
          if (ss.isEmpty)
            None
          else
            Some(
              Sequence(
                obsId,
                SequenceMetadata(i, None, sequence.title),
                ss
              )
            )
        )
      })
  }

  private def deliverObserveCmd(seqState: Sequence.State,
                                f: ObserveControl => Option[SeqAction[Unit]]): Option[Stream[IO, executeEngine.EventType]] = {
    def isObserving(v: Action): Boolean = v.kind === ActionType.Observe && (v.state.runState match {
      case Action.Started               => true
      case _                            => false
    })

    toInstrumentSys(seqState.toSequence.metadata.instrument).toOption.flatMap(x => f(x.observeControl)).flatMap {
      v => seqState.current.execution.exists(isObserving).option(Stream.eval(v.value.map(handleError)))
    }
  }

  private def handleError(t: TrySeq[Unit]): executeEngine.EventType = t match {
    case Left(e) => Event.logErrorMsg(SeqexecFailure.explain(e))
    case _       => Event.nullEvent
  }

  def stopObserve(seqId: Observation.Id)(seqState: Sequence.State): Option[Stream[IO, executeEngine.EventType]] = {
    def f(oc: ObserveControl): Option[SeqAction[Unit]] = oc match {
      case OpticControl(StopObserveCmd(stop), _, _, _, _, _) => Some(stop)
      case InfraredControl(StopObserveCmd(stop), _)          => Some(stop)
      case _                                                 => none
    }
    deliverObserveCmd(seqState, f).orElse(stopPaused(seqId)(seqState))
  }

  def abortObserve(seqId: Observation.Id)(seqState: Sequence.State): Option[Stream[IO, executeEngine.EventType]] = {
    def f(oc: ObserveControl): Option[SeqAction[Unit]] = oc match {
      case OpticControl(_, AbortObserveCmd(abort), _, _, _, _) => Some(abort)
      case InfraredControl(_, AbortObserveCmd(abort))          => Some(abort)
      case _                                                   => none
    }

    deliverObserveCmd(seqState, f).orElse(abortPaused(seqId)(seqState))
  }

  def pauseObserve(seqState: Sequence.State): Option[Stream[IO, executeEngine.EventType]] = {
    def f(oc: ObserveControl): Option[SeqAction[Unit]] = oc match {
      case OpticControl(_, _, PauseObserveCmd(pause), _, _, _) => Some(pause)
      case _                                                   => none
    }
    deliverObserveCmd( seqState, f)
  }

  private def pausedCommand(seqId: Observation.Id, f: ObserveControl => Option[Time => SeqAction[ObserveCommand.Result]])
                           (seqState: Sequence.State): Option[Stream[IO, executeEngine.EventType]] = {
    val observeIndex: Option[(ObserveContext, Int)] =
      seqState.current.execution.zipWithIndex.find(_._1.kind === ActionType.Observe).flatMap{ case (a, i) =>
        a.state.runState match {
          case Action.Paused(c: ObserveContext) => Some((c, i))
          case _                                => none
        }
      }
    def resumeIO(c: ObserveContext, resumeCmd: SeqAction[ObserveCommand.Result]): IO[Result] = (for {
      r <- resumeCmd
      ret <- c.t(r)
    } yield ret).value.map(_.toResult)

    val u: Option[Time => SeqAction[ObserveCommand.Result]] = toInstrumentSys(seqState.toSequence.metadata.instrument).toOption.flatMap(x => f(x.observeControl))
    (u, observeIndex).mapN {
      (cmd, t) => t match {
        case (c, i) => Stream.eval(IO(Event.actionResume(seqId, i, resumeIO(c, cmd(c.expTime)))))
      }
    }
  }

  def resumePaused(seqId: Observation.Id)(seqState: Sequence.State): Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case OpticControl(_, _, _, ContinuePausedCmd(a), _, _) => Some(a)
      case _                                                 => none
    }

    pausedCommand(seqId, f)(seqState)
  }

  private def stopPaused(seqId: Observation.Id)(seqState: Sequence.State): Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case OpticControl(_, _, _, _, StopPausedCmd(a), _) => Some(_ => a)
      case _                                             => none
    }

    pausedCommand(seqId, f)(seqState)
  }

  private def abortPaused(seqId: Observation.Id)(seqState: Sequence.State): Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case OpticControl(_, _, _, _, _, AbortPausedCmd(a)) => Some(_ => a)
      case _                                              => none
    }

    pausedCommand(seqId, f)(seqState)
  }

  private def toInstrumentSys(inst: Model.Instrument): TrySeq[InstrumentSystem] = inst match {
    case Model.Instrument.F2    => TrySeq(Flamingos2(systems.flamingos2))
    case Model.Instrument.GmosS => TrySeq(GmosSouth(systems.gmosSouth))
    case Model.Instrument.GmosN => TrySeq(GmosNorth(systems.gmosNorth))
    case Model.Instrument.GNIRS => TrySeq(Gnirs(systems.gnirs))
    case Model.Instrument.GPI   => TrySeq(GPI(systems.gpi))
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
    case Model.Instrument.GPI   => true
    case _                      => false
  }

  private def flatOrArcTcsSubsystems(inst: Model.Instrument): NonEmptyList[TcsController.Subsystem] = NonEmptyList.of(ScienceFold, (if (hasOI(inst)) List(OIWFS) else List.empty): _*)

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
    case Gnirs(_)      => Instrument.GNIRS
    case GPI(_)        => Instrument.GPI

  }

  private def calcInstHeader(config: Config, inst: Model.Instrument): TrySeq[Header] = {
    val tcsKReader = if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader
    inst match {
      case Model.Instrument.F2     => TrySeq(Flamingos2Header(systems.dhs, new Flamingos2Header.ObsKeywordsReaderImpl(config),
        tcsKReader))
      case Model.Instrument.GmosS |
           Model.Instrument.GmosN  =>
        val gmosInstReader = if (settings.gmosKeywords) GmosHeader.InstKeywordReaderImpl else GmosHeader.DummyInstKeywordReader
        TrySeq(GmosHeader(systems.dhs, GmosHeader.ObsKeywordsReaderImpl(config), gmosInstReader, tcsKReader))
      case Model.Instrument.GNIRS  =>
        val gnirsReader = if(settings.gnirsKeywords) GnirsKeywordReaderImpl else GnirsKeywordReaderDummy
        TrySeq(GnirsHeader(systems.dhs, gnirsReader, tcsKReader))
      case Model.Instrument.GPI    => TrySeq(GPIHeader(tcsKReader))
      case _                       => TrySeq.fail(Unexpected(s"Instrument $inst not supported."))
    }
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
                      gmosNorth: GmosController.GmosNorthController,
                      gnirs: GnirsController,
                      gpi: GPIController
                    )

  final case class Settings(
                      tcsKeywords: Boolean,
                      f2Keywords: Boolean,
                      gwsKeywords: Boolean,
                      gcalKeywords: Boolean,
                      gmosKeywords: Boolean,
                      gnirsKeywords: Boolean
                     )


  private sealed trait StepType {
    val instrument: Model.Instrument
  }

  private def extractInstrument(config: Config): TrySeq[Model.Instrument] = {
    config.extract(INSTRUMENT_KEY / INSTRUMENT_NAME_PROP).as[String].asTrySeq.flatMap {
      case Flamingos2.name => TrySeq(Model.Instrument.F2)
      case GmosSouth.name  => TrySeq(Model.Instrument.GmosS)
      case GmosNorth.name  => TrySeq(Model.Instrument.GmosN)
      case Gnirs.name      => TrySeq(Model.Instrument.GNIRS)
      case GPI.name        => TrySeq(Model.Instrument.GPI)
      case ins             => TrySeq.fail(UnrecognizedInstrument(s"inst $ins"))
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
      case Left(ConfigUtilOps.ConversionError(_, _))              => TrySeq.fail(Unexpected("Unable to get AO system from sequence"))
      case Left(ConfigUtilOps.ContentError(_))                    => TrySeq.fail(Unexpected("Logical error"))
      case Left(ConfigUtilOps.KeyNotFound(_))                     => TrySeq(CelestialObject(inst))
      case Right(AltairConstants.SYSTEM_NAME_PROP)                => TrySeq(Altair(inst))
      case Right(edu.gemini.spModel.gemini.gems.Gems.SYSTEM_NAME) => TrySeq(Gems(inst))
      case _                                                      => TrySeq.fail(Unexpected("Logical error reading AO system name"))
    }

    (config.extract(OBSERVE_KEY / OBSERVE_TYPE_PROP).as[String].leftMap(explainExtractError), extractInstrument(config)).mapN { (obsType, inst) =>
      obsType match {
        case SCIENCE_OBSERVE_TYPE                     => extractGaos(inst)
        case BIAS_OBSERVE_TYPE | DARK_OBSERVE_TYPE    => TrySeq(DarkOrBias(inst))
        case FLAT_OBSERVE_TYPE | ARC_OBSERVE_TYPE | CAL_OBSERVE_TYPE
                                                      => TrySeq(FlatOrArc(inst))
        case _                                        => TrySeq.fail(Unexpected("Unknown step type " + obsType))
      }
    }.flatten
  }

  implicit class ResponseToResult(val r: Either[SeqexecFailure, Result.Response]) extends AnyVal {
    def toResult: Result = r.fold(e => Result.Error(SeqexecFailure.explain(e)), r => Result.OK(r))
  }

  implicit class ResultToResult(val r: Either[SeqexecFailure, Result]) extends AnyVal {
    def toResult: Result = r.fold(e => Result.Error(SeqexecFailure.explain(e)), identity)
  }

  implicit class ConfigResultToResult[A <: Result.PartialVal](val r: Either[SeqexecFailure, ConfigResult]) extends AnyVal {
    def toResult: Result = r.fold(e => Result.Error(SeqexecFailure.explain(e)), r => Result.OK(Configured(r.sys.resource)))
  }

  implicit class ActionResponseToAction[A <: Result.Response](val x: SeqAction[A]) extends AnyVal {
    def toAction(kind: ActionType): Action = fromIO(kind, x.value.map(_.toResult))
  }

  implicit class ConfigResultToAction(val x: SeqAction[ConfigResult]) extends AnyVal {
    def toAction(kind: ActionType): Action = fromIO(kind, x.value.map(_.toResult))
  }

  final case class ObserveContext(t: ObserveCommand.Result => SeqAction[Result], expTime: Time) extends Result.PauseContext
}
