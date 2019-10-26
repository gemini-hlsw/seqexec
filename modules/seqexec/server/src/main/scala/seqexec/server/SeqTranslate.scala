// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.data.{EitherT, NonEmptyList, NonEmptySet}
import cats.effect.{Concurrent, IO, Timer}
import cats.effect.concurrent.Ref
import cats.implicits._
import edu.gemini.seqexec.odb.{ExecutedDataset, SeqexecSequence}
import edu.gemini.spModel.gemini.altair.AltairParams.GuideStarType
import edu.gemini.spModel.obscomp.InstConstants.DATA_LABEL_PROP
import fs2.Stream
import gem.Observation
import gem.enum.Site
import io.chrisdavenport.log4cats.Logger
import mouse.all._
import seqexec.engine._
import seqexec.engine.Action.ActionState
import seqexec.model.enum.{Instrument, Resource}
import seqexec.model._
import seqexec.model.dhs._
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
import seqexec.server.gmos.{GmosHeader, GmosNorth, GmosObsKeywordsReader, GmosSouth}
import seqexec.server.gws.GwsHeader
import seqexec.server.tcs._
import seqexec.server.tcs.TcsController.{LightPath, LightSource}
import seqexec.server.gnirs._
import seqexec.server.niri._
import seqexec.server.nifs._
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairHeader
import seqexec.server.altair.AltairLgsHeader
import seqexec.server.gems.{Gems, GemsHeader}
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigUtilOps._
import seqexec.server.gmos.NSObserveCommand
import squants.Time
import squants.time.TimeConversions._

class SeqTranslate(site: Site, systems: Systems[IO], gmosNsCmd: Ref[IO, Option[NSObserveCommand]])(implicit L: Logger[IO]) extends ObserveActions {
  import SeqTranslate._

  private def step(obsId: Observation.Id, i: StepId, config: CleanConfig, nextToRun: StepId,
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
          NonEmptyList.one(dataIdFromConfig[IO](config).flatMap(systems.odb.sequenceStart(obsId, _))
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
        ia.observeActions(env)
      }

      extractStatus(config) match {
        case StepState.Pending if i >= nextToRun => SequenceGen.PendingStepGen(
          i,
          config,
          calcResources(sys),
          StepActionsGen(initialStepExecutions, configs, rest)
        )
        case StepState.Pending                   => SequenceGen.SkippedStepGen(
          i,
          config
        )
        // TODO: This case should be for completed Steps only. Fail when step status is unknown.
        case _                                   => SequenceGen.CompletedStepGen(
          i,
          config,
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

  def sequence(obsId: Observation.Id, sequence: SeqexecSequence)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): (List[SeqexecFailure], Option[SequenceGen[IO]]) = {

    // Step Configs are wrapped in a CleanConfig to fix some known inconsistencies that can appear in the sequence
    val configs = sequence.config.getAllSteps.toList.map(CleanConfig(_))

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
    implicit tio: Timer[IO], cio: Concurrent[IO]
  ): Option[Stream[IO, executeEngine.EventType]] = {

    def isObserving[F[_]](v: Action[F]): Boolean = v.kind === ActionType.Observe && v.state.runState.started

    st.sequences.get(seqId)
      .flatMap { obsSeq =>
        (toInstrumentSys(obsSeq.seqGen.instrument).toOption,
          obsSeq.seq.currentStep.map(_.id).flatMap(x => obsSeq.seqGen.steps.find(_.id === x)).map(_.config)
          ).mapN { case (inst, cfg) => f(inst.observeControl(cfg)) }
          .flatMap { v =>
            obsSeq.seq
              .current
              .execution
              .exists(isObserving)
              .option(Stream.eval(v.attempt.map(handleError)))
          }
      }
  }

  private def handleError: Either[Throwable, Unit] => executeEngine.EventType = {
    case Left(e: SeqexecFailure) => Event.logErrorMsg(SeqexecFailure.explain(e))
    case Left(e: Throwable)      => Event.logErrorMsg(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e)))
    case _                       => Event.nullEvent
  }

  def stopObserve(seqId: Observation.Id, graceful: Boolean)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = st =>{
    def f(oc: ObserveControl[IO]): IO[Unit] = oc match {
      case CompleteControl(StopObserveCmd(stop), _, _, _, _, _) => stop(graceful)
      case UnpausableControl(StopObserveCmd(stop), _)           => stop(graceful)
      case _                                                    => IO.unit
    }
    deliverObserveCmd(seqId, f)(st).orElse(stopPaused(seqId).apply(st))
  }

  def abortObserve(seqId: Observation.Id, graceful: Boolean)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = st => {
    def f(oc: ObserveControl[IO]): IO[Unit] = oc match {
      case CompleteControl(_, AbortObserveCmd(abort), _, _, _, _) => abort(graceful)
      case UnpausableControl(_, AbortObserveCmd(abort))           => abort(graceful)
      case _                                                      => IO.unit
    }

    deliverObserveCmd(seqId, f)(st).orElse(abortPaused(seqId).apply(st))
  }

  def pauseObserve(seqId: Observation.Id, graceful: Boolean)(
    implicit tio: Timer[IO], cio: Concurrent[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(oc: ObserveControl[IO]): IO[Unit] = oc match {
      case CompleteControl(_, _, PauseObserveCmd(pause), _, _, _) => pause(graceful)
      case _                                                      => IO.unit
    }
    deliverObserveCmd(seqId, f)
  }

  def resumePaused(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = (st: EngineState) => {
    val observeIndex: Option[(ObserveContext[IO], Option[Time], Int)] =
      st.sequences.get(seqId)
        .flatMap(_.seq.current.execution.zipWithIndex.find(_._1.kind === ActionType.Observe).flatMap {
          case (a, i) => a.state.runState match {
            case ActionState.Paused(c: ObserveContext[IO]) => (c, a.state.partials.collectFirst{
              case x: Progress => x.progress}, i).some
            case _ => none
          }
        }
      )

    (st.sequences.get(seqId).flatMap{x => toInstrumentSys(x.seqGen.instrument).toOption}, observeIndex).mapN{
      case (ins, (obCtx, t, i)) => Stream.eval(IO(Event.actionResume(seqId, i,
        ins.observeProgress(obCtx.expTime, ElapsedTime(t.getOrElse(0.0.seconds)))
          .map(Result.Partial(_))
          .mergeHaltR(obCtx.resumePaused(obCtx.expTime))
          .handleErrorWith(catchObsErrors[IO])
      )))
    }
  }

  private def endPaused(seqId: Observation.Id, l: ObserveContext[IO] => Stream[IO, Result[IO]])(st: EngineState)
  : Option[Stream[IO, executeEngine.EventType]] =
    st.sequences.get(seqId)
      .flatMap(
        _.seq.current.execution.zipWithIndex.find(_._1.kind === ActionType.Observe).flatMap {
          case (a, i) => a.state.runState match {
            case ActionState.Paused(c: ObserveContext[IO]) =>
              Stream.eval(IO(Event.actionResume(seqId, i, l(c).handleErrorWith(catchObsErrors[IO])))).some
            case _                                         => none
          }
        }
      )

  private def stopPaused(seqId: Observation.Id): EngineState => Option[Stream[IO, executeEngine.EventType]] =
    endPaused(seqId, _.stopPaused)

  private def abortPaused(seqId: Observation.Id): EngineState => Option[Stream[IO, executeEngine.EventType]] =
    endPaused(seqId, _.abortPaused)

  def toInstrumentSys(inst: Instrument)(
    implicit ev: Timer[IO], cio: Concurrent[IO]
  ): TrySeq[InstrumentSystem[IO]] = inst match {
    case Instrument.F2    => TrySeq(Flamingos2(systems.flamingos2, systems.dhs))
    case Instrument.GmosS => TrySeq(GmosSouth(systems.gmosSouth, systems.dhs, gmosNsCmd))
    case Instrument.GmosN => TrySeq(GmosNorth(systems.gmosNorth, systems.dhs, gmosNsCmd))
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
                     lsource: LightSource, config: CleanConfig): TrySeq[System[IO]] = site match {
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
    config: CleanConfig,
    stepType: StepType,
    sys: InstrumentSystem[IO]
  ): TrySeq[List[System[IO]]] = {
    stepType match {
      case StepType.CelestialObject(inst)  => getTcs(
        inst.hasOI.fold(allButGaos, allButGaosNorOi),
        useGaos = false,
        sys,
        TcsController.LightSource.Sky,
        config
      ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

      case StepType.NodAndShuffle(inst)    => getTcs(
        inst.hasOI.fold(allButGaos, allButGaosNorOi),
        useGaos = false,
        sys,
        TcsController.LightSource.Sky,
        config
      ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

      case StepType.FlatOrArc(inst)        => for {
        tcs  <- getTcs(flatOrArcTcsSubsystems(inst), useGaos = false, sys, TcsController.LightSource.GCAL, config)
        gcal <- Gcal.fromConfig(systems.gcal, site == Site.GS)(config)
      } yield List(sys, tcs, gcal)

      case StepType.DarkOrBias(_) => List(sys).asRight

      case StepType.ExclusiveDarkOrBias(_) | StepType.DarkOrBiasNS(_) => List(sys, Gcal.defaultGcal[IO](systems.gcal)).asRight

      case StepType.AltairObs(inst)        => getTcs(
        inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
        useGaos = true,
        sys,
        TcsController.LightSource.AO,
        config
      ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

      case StepType.AlignAndCalib          => List(sys).asRight

      case StepType.Gems(inst)             => getTcs(
        inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
        useGaos = true,
        sys,
        TcsController.LightSource.AO,
        config
      ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

      case _                               => TrySeq.fail(Unexpected(s"Unsupported step type $stepType"))
    }
  }

  private def calcInstHeader(
    config: CleanConfig,
    sys: InstrumentSystem[IO]
  ): TrySeq[Header[IO]] = {
    sys.resource match {
      case Instrument.F2     =>
        Flamingos2Header.header[IO](sys, Flamingos2Header.ObsKeywordsReaderODB(config), systems.tcsKeywordReader).asRight
      case Instrument.GmosS |
           Instrument.GmosN  =>
        GmosHeader.header[IO](sys, GmosObsKeywordsReader(config), systems.gmosKeywordReader, systems.tcsKeywordReader).asRight
      case Instrument.Gnirs  =>
        GnirsHeader.header[IO](sys, systems.gnirsKeywordReader, systems.tcsKeywordReader).asRight
      case Instrument.Gpi    =>
        GpiHeader.header[IO](systems.gpi.gdsClient, systems.tcsKeywordReader, ObsKeywordReader[IO](config, site)).asRight
      case Instrument.Ghost  =>
        GhostHeader.header[IO].asRight
      case Instrument.Niri   =>
        NiriHeader.header[IO](sys, systems.niriKeywordReader, systems.tcsKeywordReader).asRight
      case Instrument.Nifs   =>
        NifsHeader.header[IO](sys, systems.nifsKeywordReader, systems.tcsKeywordReader).asRight
      case Instrument.Gsaoi   =>
        GsaoiHeader.header[IO](sys, systems.tcsKeywordReader, systems.gsaoiKeywordReader).asRight
      case _                 =>
        TrySeq.fail(Unexpected(s"Instrument ${sys.resource} not supported."))
    }
  }

  private def commonHeaders(config: CleanConfig, tcsSubsystems: List[TcsController.Subsystem],
                            inst: InstrumentSystem[IO])(ctx: HeaderExtraData): Header[IO] =
    new StandardHeader(
      inst,
      ObsKeywordReader[IO](config, site),
      systems.tcsKeywordReader,
      StateKeywordsReader[IO](ctx.conditions, ctx.operator, ctx.observer),
      tcsSubsystems
    )

  private def gwsHeaders(i: InstrumentSystem[IO]): Header[IO] = GwsHeader.header(i, systems.gwsKeywordReader)

  private def gcalHeader(i: InstrumentSystem[IO]): Header[IO] =
    GcalHeader.header(i, systems.gcalKeywordReader)

  private def altairHeader(instrument: InstrumentSystem[IO], tcsKReader: TcsKeywordsReader[IO]): Header[IO] =
    AltairHeader.header[IO](
      instrument,
      systems.altairKeywordReader,
      tcsKReader)

  private def altairLgsHeader(guideStar: GuideStarType, instrument: InstrumentSystem[IO]): Header[IO] =
    if (guideStar === GuideStarType.LGS) {
      AltairLgsHeader.header(instrument, systems.altairKeywordReader)
    } else {
      dummyHeader[IO]
    }

  private def gemsHeaders(instrument: InstrumentSystem[IO],
                          obsKReader: ObsKeywordsReader[IO],
                          tcsKReader: TcsKeywordsReader[IO])
  : Header[IO] = GemsHeader.header[IO](
    instrument,
    systems.gemsKeywordsReader,
    obsKReader,
    tcsKReader
  )

  private def calcHeaders(
    config: CleanConfig,
    stepType: StepType,
    sys: InstrumentSystem[IO]
  ): TrySeq[HeaderExtraData => List[Header[IO]]] = {
    stepType match {
      case StepType.CelestialObject(_) | StepType.NodAndShuffle(_) =>
          calcInstHeader(config, sys).map(h => ctx =>
            List(commonHeaders(config, allButGaos.toList, sys)(ctx), gwsHeaders(sys), h))

      case StepType.AltairObs(_)    =>
        for {
          gst  <- Altair.guideStarType(config)
          read <- calcInstHeader(config, sys).map(h => (ctx: HeaderExtraData) =>
                    // Order is important
                    List(
                      commonHeaders(config, allButGaos.toList, sys)(ctx),
                      altairHeader(sys, systems.tcsKeywordReader),
                      altairLgsHeader(gst, sys),
                      gwsHeaders(sys), h))
        } yield read

      case StepType.FlatOrArc(inst) =>
          calcInstHeader(config, sys).map(h => ctx =>
            List(commonHeaders(config, flatOrArcTcsSubsystems(inst).toList, sys)(ctx), gcalHeader(sys), gwsHeaders(sys), h))

      case StepType.DarkOrBias(_) | StepType.DarkOrBiasNS(_) | StepType.ExclusiveDarkOrBias(_) =>
          calcInstHeader(config, sys).map(h => (ctx => List(commonHeaders(config, Nil, sys)(ctx), gwsHeaders(sys), h)))

      case StepType.AlignAndCalib   => TrySeq(_ => Nil) // No headers for A&C

      case StepType.Gems(_)         =>
        val obsKReader = ObsKeywordReader[IO](config, site)
        calcInstHeader(config, sys).map(h => ctx =>
          List(commonHeaders(config, allButGaos.toList, sys)(ctx),
            gwsHeaders(sys),
            gemsHeaders(sys, obsKReader, systems.tcsKeywordReader), h
          )
        )

      case st                       => TrySeq.fail(Unexpected(s"Unsupported step type $st"))
    }
  }

}

object SeqTranslate {
  def apply(site: Site, systems: Systems[IO])(implicit L: Logger[IO]): IO[SeqTranslate] =
    Ref.of[IO, Option[NSObserveCommand]](none).map((new SeqTranslate(site, systems, _)))

  def dataIdFromConfig[F[_]: MonadError[?[_], Throwable]](config: CleanConfig): F[DataId] =
    EitherT
      .fromEither[F](
        config
          .extractObsAs[String](DATA_LABEL_PROP)
          .map(toDataId)
          .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
      )
      .widenRethrowT

}
