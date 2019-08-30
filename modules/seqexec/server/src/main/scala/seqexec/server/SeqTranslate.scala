// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.data.NonEmptySet
import cats.data.NonEmptyList
import cats.effect.{Concurrent, IO, Timer}
import cats.effect.Sync
import cats.effect.LiftIO
import cats.implicits._
import edu.gemini.seqexec.odb.{ExecutedDataset, SeqexecSequence}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.altair.AltairParams.GuideStarType
import fs2.Stream
import gem.Observation
import gem.enum.Site
import io.chrisdavenport.log4cats.Logger
import mouse.all._
import seqexec.engine._
import seqexec.engine.Action.ActionState
import seqexec.model.enum.{Instrument, Resource}
import seqexec.model.enum.ObserveCommandResult
import seqexec.model._
import seqexec.model.dhs._
import seqexec.server.ConfigUtilOps._
import seqexec.server.SeqexecFailure.Unexpected
import seqexec.server.InstrumentSystem._
import seqexec.server.SequenceGen.StepActionsGen
import seqexec.server.flamingos2.{Flamingos2, Flamingos2Header}
import seqexec.server.keywords._
import seqexec.server.SequenceConfiguration._
import seqexec.server.gpi.{Gpi, GpiHeader}
import seqexec.server.ghost.{Ghost, GhostHeader}
import seqexec.server.gsaoi._
import seqexec.server.gcal._
import seqexec.server.gmos.{GmosEpics, GmosHeader, GmosKeywordReaderDummy, GmosKeywordReaderEpics, GmosNorth, GmosObsKeywordsReader, GmosSouth}
import seqexec.server.gws.{DummyGwsKeywordsReader, GwsEpics, GwsHeader, GwsKeywordsReaderEpics}
import seqexec.server.tcs._
import seqexec.server.tcs.TcsController.{LightPath, LightSource}
import seqexec.server.gnirs._
import seqexec.server.niri._
import seqexec.server.nifs._
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairHeader
import seqexec.server.altair.AltairEpics
import seqexec.server.altair.AltairLgsHeader
import seqexec.server.altair.AltairKeywordReaderEpics
import seqexec.server.altair.AltairKeywordReaderDummy
import seqexec.server.gems.{Gems, GemsEpics, GemsHeader, GemsKeywordReaderDummy, GemsKeywordReaderEpics}
import squants.Time
import squants.time.TimeConversions._

class SeqTranslate(site: Site, systems: Systems[IO], settings: TranslateSettings)(implicit L: Logger[IO]) extends ObserveActions {
  import SeqTranslate._

  private def step(obsId: Observation.Id, i: StepId, config: Config, nextToRun: StepId,
                   datasets: Map[Int, ExecutedDataset])(
                     implicit cio: Concurrent[IO],
                              tio: Timer[IO]
                   ): TrySeq[SequenceGen.StepGen[IO]] = {
    def buildStep(
      inst: InstrumentSystem[IO],
      sys: List[System[IO]],
      headers: HeaderExtraData => List[Header[IO]],
      stepType: StepType
    ): SequenceGen.StepGen[IO] = {
      val ia = inst.instrumentActions(config)
      val initialStepExecutions: List[ParallelActions[IO]] =
        // Ask the instrument if we need an initial action
        (i === 0 && ia.runInitialAction(stepType)).option {
          NonEmptyList.one(systems.odb.sequenceStart(obsId, toImageFileId(""))
            .as(Response.Ignored).toAction(ActionType.Undefined))
        }.toList

      val configs: Map[Resource, Action[IO]] = sys.map { x =>
        val res = x.resource
        val kind = ActionType.Configure(res)

        res -> x.configure(config).as(Response.Configured(res)).toAction(kind)
      }.toMap

      def rest(ctx: HeaderExtraData): List[ParallelActions[IO]] = {
        val env = ObserveEnvironment(systems, config, stepType, obsId, inst, sys.filterNot(inst.equals), headers, ctx)
        // Request the instrument to build the observe actions and merge them with the progress
        // Also catches any errors in the process of runnig an observation
        ia.observeActions(env, (s, env) =>
          ia.observationProgressStream(env)
            .mergeHaltR(s)
            .handleErrorWith(catchObsErrors[IO]))
      }

      extractStatus(config) match {
        case StepState.Pending if i >= nextToRun => SequenceGen.PendingStepGen(
          i,
          config.toStepConfig,
          calcResources(sys),
          StepActionsGen(initialStepExecutions, configs, rest)
        )
        case StepState.Pending                   => SequenceGen.SkippedStepGen(
          i,
          config.toStepConfig
        )
        // TODO: This case should be for completed Steps only. Fail when step status is unknown.
        case _                                   => SequenceGen.CompletedStepGen(
          i,
          config.toStepConfig,
          datasets.get(i + 1).map(_.filename).map(toImageFileId)
        )
      }
    }

    for {
      inst      <- extractInstrument(config)
      is        <- toInstrumentSys(inst)
      stepType  <- is.calcStepType(config)
      systems   <- calcSystems(config, stepType, is)
      headers   <- calcHeaders(config, stepType, is)
    } yield buildStep(is, systems, headers, stepType)
  }

  def catchObsErrors[F[_]](t: Throwable): Stream[F, Result[F]] = t match {
    case e: SeqexecFailure =>
      Stream.emit(Result.Error(SeqexecFailure.explain(e)))
    case e: Throwable =>
      Stream.emit(Result.Error(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e))))
  }

  def sequence(obsId: Observation.Id, sequence: SeqexecSequence)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): (List[SeqexecFailure], Option[SequenceGen[IO]]) = {

    val configs = sequence.config.getAllSteps.toList

    val nextToRun = configs
      .map(extractStatus)
      .lastIndexWhere(s => s === StepState.Completed || s === StepState.Skipped) + 1

    val steps = configs.zipWithIndex.map {
      case (c, i) => step(obsId, i, c, nextToRun, sequence.datasets)
    }.separate

    val instName = configs
      .headOption
      .map(extractInstrument)
      .getOrElse(Either.left(SeqexecFailure.UnrecognizedInstrument("UNKNOWN")))

    instName.fold(e => (List(e), none), i =>
      steps match {
        case (errs, ss) => (
          errs,
          ss.headOption.map { _ =>
            SequenceGen(
              obsId,
              sequence.title,
              i,
              ss
            )
          }
        )
      })
  }

  private def deliverObserveCmd(seqId: Observation.Id, f: ObserveControl[IO] => IO[Unit])(st: EngineState)(
    implicit tio: Timer[IO]
  ): Option[Stream[IO, executeEngine.EventType]] = {
    def isObserving(v: Action[IO]): Boolean = v.kind === ActionType.Observe && (v.state.runState match {
      case ActionState.Started => true
      case _                   => false
    })

    def seqCmd(seqState: Sequence.State[IO], instrument: Instrument): Stream[IO, executeEngine.EventType] =
      toInstrumentSys(instrument) match {
        case Right(x) =>
          if (seqState.current.execution.exists(isObserving)) Stream.eval(f(x.observeControl).attempt.map(handleError)) else Stream.empty[IO]
        case Left(_) => Stream.empty[IO]
      }
    (st.sequences.seq.get(seqId), st.sequences.get(seqId)).mapN { (seqg, obsseq) =>
      seqCmd(obsseq.seq, seqg.seqGen.instrument)
    }

  }

  private def handleError: Either[Throwable, Unit] => executeEngine.EventType = {
    case Left(e: SeqexecFailure) => Event.logErrorMsg(SeqexecFailure.explain(e))
    case Left(e: Throwable) => Event.logErrorMsg(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e)))
    case _       => Event.nullEvent
  }

  def stopObserve(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = st =>{
    def f(oc: ObserveControl[IO]): IO[Unit] = oc match {
      case CompleteControl(StopObserveCmd(stop), _, _, _, _, _) => stop
      case UnpausableControl(StopObserveCmd(stop), _)           => stop
      case _                                                    => IO.unit
    }
    deliverObserveCmd(seqId, f)(st).orElse(stopPaused(seqId).apply(st))
  }

  def abortObserve(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = st => {
    def f(oc: ObserveControl[IO]): IO[Unit] = oc match {
      case CompleteControl(_, AbortObserveCmd(abort), _, _, _, _) => abort
      case UnpausableControl(_, AbortObserveCmd(abort))           => abort
      case _                                                      => IO.unit
    }

    deliverObserveCmd(seqId, f)(st).orElse(abortPaused(seqId).apply(st))
  }

  def pauseObserve(seqId: Observation.Id)(
    implicit tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(oc: ObserveControl[IO]): IO[Unit] = oc match {
      case CompleteControl(_, _, PauseObserveCmd(pause), _, _, _) => pause
      case _                                                      => IO.unit
    }
    deliverObserveCmd(seqId, f)
  }

  private def pausedCommand(seqId: Observation.Id,
                            f: ObserveControl[IO] => Option[Time => IO[ObserveCommandResult]],
                            useCountdown: Boolean)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = st => {

    def resumeIO(c: ObserveContext[IO], resumeCmd: IO[ObserveCommandResult]): Stream[IO, Result[IO]] =
      for {
        r <- Stream.eval(resumeCmd)
        ret <- c.t(r)
      } yield ret

    def seqCmd(seqState: Sequence.State[IO], instrument: Instrument): Option[Stream[IO,
      executeEngine.EventType]] = {

      val inst = toInstrumentSys(instrument).toOption

      val observeIndex: Option[(ObserveContext[IO], Option[Time], Int)] =
        seqState.current.execution.zipWithIndex.find(_._1.kind === ActionType.Observe).flatMap {
          case (a, i) => a.state.runState match {
            case ActionState.Paused(c: ObserveContext[IO]) => (c, a.state.partials.collectFirst{
              case x@Progress(_, _) => x.progress}, i).some
            case _ => none
          }
        }

      val u: Option[Time => IO[ObserveCommandResult]] =
        inst.flatMap(x => f(x.observeControl))

      (u, observeIndex, inst).mapN {
        (cmd, t, ins) =>
          t match {
            case (c, to, i) =>
              if(useCountdown)
                Stream.eval(IO(Event.actionResume(seqId, i,
                  ins.observeProgress(c.expTime, ElapsedTime(to.getOrElse(0.0.seconds)))
                    .map(Result.Partial(_))
                    .mergeHaltR(resumeIO(c, cmd(c.expTime)))
                )))
              else
                Stream.eval(IO(Event.actionResume(seqId, i,
                  resumeIO(c, cmd(c.expTime)))))
          }
      }
    }

    for {
      seqg   <- st.sequences.seq.get(seqId)
      obsseq <- st.sequences.get(seqId)
      r      <- seqCmd(obsseq.seq, seqg.seqGen.instrument)
    } yield r
  }

  def resumePaused(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl[IO]): Option[Time => IO[ObserveCommandResult]] = o match {
      case CompleteControl(_, _, _, ContinuePausedCmd(a), _, _) => a.some
      case _                                                    => none
    }

    pausedCommand(seqId, f, useCountdown = true)
  }

  private def stopPaused(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl[IO]): Option[Time => IO[ObserveCommandResult]] = o match {
      case CompleteControl(_, _, _, _, StopPausedCmd(a), _) => Some(_ => a)
      case _                                                => none
    }

    pausedCommand(seqId, f, useCountdown = false)
  }

  private def abortPaused(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl[IO]): Option[Time => IO[ObserveCommandResult]] = o match {
      case CompleteControl(_, _, _, _, _, AbortPausedCmd(a)) => Some(_ => a)
      case _                                                 => none
    }

    pausedCommand(seqId, f, useCountdown = false)
  }

  def toInstrumentSys(inst: Instrument)(
    implicit ev: Timer[IO]
  ): TrySeq[InstrumentSystem[IO]] = inst match {
    case Instrument.F2    => TrySeq(Flamingos2(systems.flamingos2, systems.dhs))
    case Instrument.GmosS => TrySeq(GmosSouth(systems.gmosSouth, systems.dhs))
    case Instrument.GmosN => TrySeq(GmosNorth(systems.gmosNorth, systems.dhs))
    case Instrument.Gnirs => TrySeq(Gnirs(systems.gnirs, systems.dhs))
    case Instrument.Gpi   => TrySeq(Gpi(systems.gpi))
    case Instrument.Ghost => TrySeq(Ghost(systems.ghost))
    case Instrument.Niri  => TrySeq(Niri(systems.niri, systems.dhs))
    case Instrument.Nifs  => TrySeq(Nifs(systems.nifs, systems.dhs))
    case Instrument.Gsaoi => TrySeq(Gsaoi(systems.gsaoi, systems.dhs))
    case _                => TrySeq.fail(Unexpected(s"Instrument $inst not supported."))
  }

  private def calcResources[F[_]](sys: List[System[F]]): Set[Resource] =
    sys.map(_.resource).toSet

  import TcsController.Subsystem._

  private def flatOrArcTcsSubsystems(inst: Instrument): NonEmptySet[TcsController.Subsystem] =
    NonEmptySet.of(AGUnit, (if (inst.hasOI) List(OIWFS) else List.empty): _*)

  private def getTcs(subs: NonEmptySet[TcsController.Subsystem], useGaos: Boolean, inst: InstrumentSystem[IO],
                     lsource: LightSource, config: Config): TrySeq[System[IO]] = site match {
    case Site.GS => if(useGaos)
      Gems.fromConfig[IO](systems.gems, systems.guideDb)(config).map(a =>
        TcsSouth.fromConfig[IO](systems.tcsSouth, subs, a.some, inst, systems.guideDb)(
          config, LightPath(lsource, inst.sfName(config)), extractWavelength(config)
        )
      )
      else
        TcsSouth.fromConfig[IO](systems.tcsSouth, subs, None, inst, systems.guideDb)(
          config, LightPath(lsource, inst.sfName(config)), extractWavelength(config)
        ).asRight
    case Site.GN => if(useGaos)
        Altair.fromConfig(config, systems.altair).map(a =>
          TcsNorth.fromConfig[IO](systems.tcsNorth, subs, a.some, inst, systems.guideDb)(
            config, LightPath(lsource, inst.sfName(config)), extractWavelength(config)
          )
        )
      else
        TcsNorth.fromConfig[IO](systems.tcsNorth, subs, none, inst, systems.guideDb)(
          config, LightPath(lsource, inst.sfName(config)), extractWavelength(config)
        ).asRight
  }

  private def calcSystems(
    config: Config,
    stepType: StepType,
    sys: InstrumentSystem[IO]
  ): TrySeq[List[System[IO]]] = {
    stepType match {
      case StepType.CelestialObject(inst) => getTcs(
        inst.hasOI.fold(allButGaos, allButGaosNorOi),
        useGaos = false,
        sys,
        TcsController.LightSource.Sky,
        config
      ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

      case StepType.NodAndShuffle(inst)   => getTcs(
        inst.hasOI.fold(allButGaos, allButGaosNorOi),
        useGaos = false,
        sys,
        TcsController.LightSource.Sky,
        config
      ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

      case StepType.FlatOrArc(inst)       => for {
        tcs  <- getTcs(flatOrArcTcsSubsystems(inst), useGaos = false, sys, TcsController.LightSource.GCAL, config)
        gcal <- Gcal.fromConfig(systems.gcal, site == Site.GS)(config)
      } yield List(sys, tcs, gcal)

      case StepType.DarkOrBias(_)         =>  List(sys).asRight

      case StepType.AltairObs(inst)       => getTcs(
        inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
        useGaos = true,
        sys,
        TcsController.LightSource.AO,
        config
      ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

      case StepType.AlignAndCalib         => List(sys).asRight

      case StepType.Gems(inst)            => getTcs(
        inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
        useGaos = true,
        sys,
        TcsController.LightSource.AO,
        config
      ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

      case _                              => TrySeq.fail(Unexpected(s"Unsupported step type $stepType"))
    }
  }

  private def calcInstHeader(
    config: Config,
    sys: InstrumentSystem[IO]
  ): TrySeq[Header[IO]] = {
    val tcsKReader = if (settings.tcsKeywords) TcsKeywordsReaderEpics[IO](TcsEpics.instance) else DummyTcsKeywordsReader[IO]
    sys.resource match {
      case Instrument.F2     =>
        Flamingos2Header.header[IO](sys, Flamingos2Header.ObsKeywordsReaderODB(config), tcsKReader).asRight
      case Instrument.GmosS |
           Instrument.GmosN  =>
        val gmosInstReader = if (settings.gmosKeywords) GmosKeywordReaderEpics[IO](GmosEpics.instance) else GmosKeywordReaderDummy[IO]
        GmosHeader.header[IO](sys, GmosObsKeywordsReader(config), gmosInstReader, tcsKReader).asRight
      case Instrument.Gnirs  =>
        val gnirsReader = if(settings.gnirsKeywords) GnirsKeywordReaderEpics[IO](GnirsEpics.instance) else GnirsKeywordReaderDummy[IO]
        GnirsHeader.header[IO](sys, gnirsReader, tcsKReader).asRight
      case Instrument.Gpi    =>
        GpiHeader.header[IO](systems.gpi.gdsClient, tcsKReader, ObsKeywordReader[IO](config, site)).asRight
      case Instrument.Ghost  =>
        GhostHeader.header[IO].asRight
      case Instrument.Niri   =>
        val niriReader = if(settings.niriKeywords) NiriKeywordReaderEpics[IO](NiriEpics.instance)
                          else NiriKeywordReaderDummy[IO]
        NiriHeader.header[IO](sys, niriReader, tcsKReader).asRight
      case Instrument.Nifs   =>
        val nifsReader = if(settings.nifsKeywords) NifsKeywordReaderEpics[IO](NifsEpics.instance) else NifsKeywordReaderDummy[IO]
        NifsHeader.header[IO](sys, nifsReader, tcsKReader).asRight
      case Instrument.Gsaoi   =>
        val gsaoiReader = if (settings.gsaoiKeywords) GsaoiKeywordReaderEpics[IO](GsaoiEpics.instance) else GsaoiKeywordReaderDummy[IO]
        GsaoiHeader.header[IO](sys, tcsKReader, gsaoiReader).asRight
      case _                 =>
        TrySeq.fail(Unexpected(s"Instrument ${sys.resource} not supported."))
    }
  }

  private def commonHeaders[F[_]: Sync](epics: => TcsEpics[F], config: Config, tcsSubsystems: List[TcsController.Subsystem],
                            inst: InstrumentSystem[F])(ctx: HeaderExtraData): Header[F] =
    new StandardHeader(
      inst,
      ObsKeywordReader[F](config, site),
      if (settings.tcsKeywords) TcsKeywordsReaderEpics[F](epics) else DummyTcsKeywordsReader[F],
      StateKeywordsReader[F](ctx.conditions, ctx.operator, ctx.observer),
      tcsSubsystems
    )

  private def gwsHeaders[F[_]: Sync](epics: => GwsEpics[F], i: InstrumentSystem[F]): Header[F] = GwsHeader.header(i,
    if (settings.gwsKeywords) GwsKeywordsReaderEpics[F](epics) else DummyGwsKeywordsReader[F])

  private def gcalHeader[F[_]: Sync](epics: => GcalEpics[F], i: InstrumentSystem[F]): Header[F] = GcalHeader.header(i,
    if (settings.gcalKeywords) GcalKeywordsReaderEpics[F](epics) else DummyGcalKeywordsReader[F] )

  private def altairHeader[F[_]: Sync: LiftIO](epics: => AltairEpics[F], instrument: InstrumentSystem[F], tcsKReader: TcsKeywordsReader[F]): Header[F] =
    AltairHeader.header[F](
      instrument,
      if (settings.altairKeywords) AltairKeywordReaderEpics[F](epics) else AltairKeywordReaderDummy[F],
      tcsKReader)

  private def altairLgsHeader[F[_]: Sync](epics: => AltairEpics[F], guideStar: GuideStarType, instrument: InstrumentSystem[F]): Header[F] =
    if (guideStar === GuideStarType.LGS) {
      AltairLgsHeader.header(instrument,
        if (settings.altairKeywords) AltairKeywordReaderEpics[F](epics) else AltairKeywordReaderDummy[F])
    } else {
      dummyHeader[F]
    }

  private def gemsHeaders[F[_]: Sync](epics: => GemsEpics[F],
                                      gsaoiEpics: => GsaoiEpics[F],
                                      instrument: InstrumentSystem[F],
                                      obsKReader: ObsKeywordsReader[F],
                                      tcsKReader: TcsKeywordsReader[F])
  : Header[F] = GemsHeader.header[F](
    instrument,
    if(settings.gemsKeywords && settings.gsaoiKeywords) GemsKeywordReaderEpics[F](epics, gsaoiEpics)
    else GemsKeywordReaderDummy[F],
    obsKReader,
    tcsKReader
  )

  private def calcHeaders(
    config: Config,
    stepType: StepType,
    sys: InstrumentSystem[IO]
  ): TrySeq[HeaderExtraData => List[Header[IO]]] = {
    stepType match {
      case StepType.CelestialObject(_) | StepType.NodAndShuffle(_) =>
          calcInstHeader(config, sys).map(h => ctx =>
            List(commonHeaders(TcsEpics.instance, config, allButGaos.toList, sys)(ctx), gwsHeaders(GwsEpics.instance, sys), h))

      case StepType.AltairObs(_)    =>
        val tcsKReader = if (settings.tcsKeywords) TcsKeywordsReaderEpics[IO](TcsEpics.instance) else DummyTcsKeywordsReader[IO]
        for {
          gst  <- Altair.guideStarType(config)
          read <- calcInstHeader(config, sys).map(h => (ctx: HeaderExtraData) =>
                    // Order is important
                    List(
                      commonHeaders(TcsEpics.instance, config, allButGaos.toList, sys)(ctx),
                      altairHeader(AltairEpics.instance, sys, tcsKReader),
                      altairLgsHeader(AltairEpics.instance, gst, sys),
                      gwsHeaders(GwsEpics.instance, sys), h))
        } yield read

      case StepType.FlatOrArc(inst) =>
          calcInstHeader(config, sys).map(h => ctx =>
            List(commonHeaders(TcsEpics.instance, config, flatOrArcTcsSubsystems(inst).toList, sys)(ctx), gcalHeader(GcalEpics.instance, sys), gwsHeaders(GwsEpics.instance, sys), h))

      case StepType.DarkOrBias(_)   =>
          calcInstHeader(config, sys).map(h => (ctx => List(commonHeaders(TcsEpics.instance, config, Nil, sys)(ctx), gwsHeaders(GwsEpics.instance, sys), h)))

      case StepType.AlignAndCalib   => TrySeq(_ => Nil) // No headers for A&C

      case StepType.Gems(_)         =>
        val tcsKReader = if (settings.tcsKeywords) TcsKeywordsReaderEpics[IO](TcsEpics.instance) else DummyTcsKeywordsReader[IO]
        val obsKReader = ObsKeywordReader[IO](config, site)
        calcInstHeader(config, sys).map(h => ctx =>
          List(commonHeaders(TcsEpics.instance, config, allButGaos.toList, sys)(ctx),
            gwsHeaders(GwsEpics.instance, sys),
            gemsHeaders(GemsEpics.instance, GsaoiEpics.instance, sys, obsKReader, tcsKReader),h
          )
        )

      case st                       => TrySeq.fail(Unexpected(s"Unsupported step type $st"))
    }
  }

}

object SeqTranslate {
  def apply(site: Site, systems: Systems[IO], settings: TranslateSettings)(implicit L: Logger[IO]): SeqTranslate =
    new SeqTranslate(site, systems, settings)

  implicit class ResponseToResult(val r: Either[Throwable, Response]) extends AnyVal {
    def toResult[F[_]]: Result[F] = r.fold(e => e match {
      case e: SeqexecFailure => Result.Error(SeqexecFailure.explain(e))
      case e: Throwable      => Result.Error(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e)))
    }, r => Result.OK(r))
  }

  implicit class ResultToResult[F[_]](val r: Either[SeqexecFailure, Result[F]]) extends AnyVal {
    def toResult: Result[F] = r.fold(e => Result.Error(SeqexecFailure.explain(e)), identity)
  }

  implicit class ActionResponseToAction[F[_]: Functor: ApplicativeError[?[_], Throwable], A <: Response](val x: F[A]) {
    def toAction(kind: ActionType): Action[F] = fromF[F](kind, x.attempt.map(_.toResult))
  }

  implicit class ConfigResultToAction[F[_]: Functor](val x: F[ConfigResult[F]]) {
    def toAction(kind: ActionType): Action[F] = fromF[F](kind, x.map(r => Result.OK(Response.Configured(r.sys.resource))))
  }

}
