// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.data.{EitherT, NonEmptyList, NonEmptySet}
import cats.effect.{Concurrent, Sync, Timer}
import cats.effect.concurrent.Ref
import cats.implicits._
import edu.gemini.seqexec.odb.{ExecutedDataset, SeqexecSequence}
import edu.gemini.spModel.gemini.altair.AltairParams.GuideStarType
import edu.gemini.spModel.obscomp.InstConstants.{DATA_LABEL_PROP, OBSERVE_TYPE_PROP, SCIENCE_OBSERVE_TYPE}
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
import scala.concurrent.duration._

trait SeqTranslate[F[_]] extends ObserveActions {

  def sequence(obsId: Observation.Id, sequence: SeqexecSequence)(
    implicit cio: Concurrent[F], tio: Timer[F]
  ): F[(List[Throwable], Option[SequenceGen[F]])]

  def stopObserve(seqId: Observation.Id, graceful: Boolean)(
    implicit cio: Concurrent[F], tio: Timer[F]
  ): EngineState[F] => Option[Stream[F, EventType[F]]]

  def abortObserve(seqId: Observation.Id)(
    implicit cio: Concurrent[F], tio: Timer[F]
  ): EngineState[F] => Option[Stream[F, EventType[F]]]

  def pauseObserve(seqId: Observation.Id, graceful: Boolean)(
    implicit tio: Timer[F], cio: Concurrent[F]
  ): EngineState[F] => Option[Stream[F, EventType[F]]]

  def resumePaused(seqId: Observation.Id)(
    implicit cio: Concurrent[F], tio: Timer[F]
  ): EngineState[F] => Option[Stream[F, EventType[F]]]

}

object SeqTranslate {
  private class SeqTranslateImpl[F[_]: Sync: Logger](site: Site,
                                                     systems: Systems[F],
                                                     gmosNsCmd: Ref[F, Option[NSObserveCommand]]
                                                    ) extends SeqTranslate[F] {

    private def step(obsId: Observation.Id, i: StepId, config: CleanConfig, nextToRun: StepId,
                     datasets: Map[Int, ExecutedDataset], isNightSeq: Boolean)(
                       implicit cio: Concurrent[F],
                                tio: Timer[F]
                     ): F[SequenceGen.StepGen[F]] = {
      def buildStep(
        inst: InstrumentSystem[F],
        sys: List[System[F]],
        headers: HeaderExtraData => List[Header[F]],
        stepType: StepType
      ): SequenceGen.StepGen[F] = {
        val ia = inst.instrumentActions(config)
        val initialStepExecutions: List[ParallelActions[F]] =
          // Ask the instrument if we need an initial action
          (i === 0 && ia.runInitialAction(stepType)).option {
            NonEmptyList.one(dataIdFromConfig[F](config).flatMap(systems.odb.sequenceStart(obsId, _))
              .as(Response.Ignored).toAction(ActionType.Undefined))
          }.toList

        val configs: Map[Resource, Action[F]] = sys.map { x =>
          val res = x.resource
          val kind = ActionType.Configure(res)

          res -> x.configure(config).as(Response.Configured(res)).toAction(kind)
        }.toMap

        def rest(ctx: HeaderExtraData): List[ParallelActions[F]] = {
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
        inst      <- MonadError[F, Throwable].fromEither(extractInstrument(config))
        is        <- toInstrumentSys(inst)
        stepType  <- is.calcStepType(config, isNightSeq).fold(_.raiseError[F, StepType], _.pure[F])
        systems   <- calcSystems(config, stepType, is)
        headers   <- calcHeaders(config, stepType, is)
      } yield buildStep(is, systems, headers, stepType)
    }

    override def sequence(obsId: Observation.Id, sequence: SeqexecSequence)(
      implicit cio: Concurrent[F],
               tio: Timer[F]
    ): F[(List[Throwable], Option[SequenceGen[F]])] = {

      // Step Configs are wrapped in a CleanConfig to fix some known inconsistencies that can appear in the sequence
      val configs = sequence.config.getAllSteps.toList.map(CleanConfig(_))

      val isNightSeq: Boolean = configs.exists(
        _.extractObsAs[String](OBSERVE_TYPE_PROP).exists(_ === SCIENCE_OBSERVE_TYPE)
      )

      val nextToRun = configs
        .map(extractStatus)
        .lastIndexWhere(_.isFinished) + 1

      val steps = configs.zipWithIndex.map {
        case (c, i) => step(obsId, i, c, nextToRun, sequence.datasets, isNightSeq).attempt
      }.sequence.map(_.separate)

      val instName = configs
        .headOption
        .map(extractInstrument)
        .getOrElse(Either.left(SeqexecFailure.UnrecognizedInstrument("UNKNOWN")))

      steps.map { sts =>
        instName.fold(e => (List(e), none), i =>
          sts match {
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
    }

    private def deliverObserveCmd(seqId: Observation.Id, f: ObserveControl[F] => F[Unit])(st: EngineState[F])(
      implicit tio: Timer[F], cio: Concurrent[F]
    ): Option[Stream[F, EventType[F]]] = {

      def isObserving(v: Action[F]): Boolean = v.kind === ActionType.Observe && v.state.runState.started

      for {
        obsSeq <- st.sequences.get(seqId)
        stId   <- obsSeq.seq.currentStep.map(_.id)
        cfg    <- obsSeq.seqGen.steps.find(_.id === stId).map(_.config)
        a = cfg //workaround for scala/bug#11175
        if obsSeq.seq
          .current
          .execution
          .exists(isObserving)
      } yield Stream.eval(
        toInstrumentSys(obsSeq.seqGen.instrument)
          .flatMap{ i =>
            f(i.observeControl(cfg))
              .attempt
              .flatMap(handleError)
          }
      )
    }

    private def handleError: Either[Throwable, Unit] => F[EventType[F]] = {
      case Left(e: SeqexecFailure) => Event.logErrorMsgF(SeqexecFailure.explain(e))
      case Left(e: Throwable)      => Event.logErrorMsgF(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e)))
      case _                       => Event.nullEvent[F].pure[F].widen[EventType[F]]
    }

    override def stopObserve(seqId: Observation.Id, graceful: Boolean)(
      implicit cio: Concurrent[F],
               tio: Timer[F]
    ): EngineState[F] => Option[Stream[F, EventType[F]]] = st => {
      def f(oc: ObserveControl[F]): F[Unit] = oc match {
        case CompleteControl(StopObserveCmd(stop), _, _, _, _, _) => stop(graceful)
        case UnpausableControl(StopObserveCmd(stop), _)           => stop(graceful)
        case _                                                    => Applicative[F].unit
      }
      deliverObserveCmd(seqId, f)(st).orElse(stopPaused(seqId).apply(st))
    }

    override def abortObserve(seqId: Observation.Id)(
      implicit cio: Concurrent[F],
               tio: Timer[F]
    ): EngineState[F] => Option[Stream[F, EventType[F]]] = st => {
      def f(oc: ObserveControl[F]): F[Unit] = oc match {
        case CompleteControl(_, AbortObserveCmd(abort), _, _, _, _) => abort
        case UnpausableControl(_, AbortObserveCmd(abort))           => abort
        case _                                                      => Applicative[F].unit
      }

      deliverObserveCmd(seqId, f)(st).orElse(abortPaused(seqId).apply(st))
    }

    override def pauseObserve(seqId: Observation.Id, graceful: Boolean)(
      implicit tio: Timer[F], cio: Concurrent[F]
    ): EngineState[F] => Option[Stream[F, EventType[F]]] = {
      def f(oc: ObserveControl[F]): F[Unit] = oc match {
        case CompleteControl(_, _, PauseObserveCmd(pause), _, _, _) => pause(graceful)
        case _                                                      => Applicative[F].unit
      }
      deliverObserveCmd(seqId, f)
    }

    override def resumePaused(seqId: Observation.Id)(
      implicit cio: Concurrent[F],
               tio: Timer[F]
    ): EngineState[F] => Option[Stream[F, EventType[F]]] = (st: EngineState[F]) => {
      val observeIndex: Option[(ObserveContext[F], Option[Time], Int)] =
        st.sequences.get(seqId)
          .flatMap(_.seq.current.execution.zipWithIndex.find(_._1.kind === ActionType.Observe).flatMap {
            case (a, i) => a.state.runState match {
              case ActionState.Paused(c: ObserveContext[F]) => (c, a.state.partials.collectFirst{
                case x: Progress => x.progress}, i).some
              case _ => none
            }
          }
        )

      (st.sequences.get(seqId), observeIndex).mapN{
        case (seq, (obCtx, t, i)) => Stream.eval(
          toInstrumentSys(seq.seqGen.instrument)
            .map{ ins =>
              Event.actionResume[F, EngineState[F], SeqEvent](
                seqId,
                i,
                ins.observeProgress(obCtx.expTime, ElapsedTime(t.getOrElse(0.0.seconds)))
                  .map(Result.Partial(_))
                  .widen[Result[F]]
                  .mergeHaltR(obCtx.resumePaused(obCtx.expTime))
                  .handleErrorWith(catchObsErrors[F])
              )
            }
        )
      }
    }

    private def endPaused(seqId: Observation.Id, l: ObserveContext[F] => Stream[F, Result[F]])(st: EngineState[F])
    : Option[Stream[F, EventType[F]]] =
      st.sequences.get(seqId)
        .flatMap(
          _.seq.current.execution.zipWithIndex.find(_._1.kind === ActionType.Observe).flatMap {
            case (a, i) => a.state.runState match {
              case ActionState.Paused(c: ObserveContext[F]) =>
                Stream.eval(Event.actionResume(seqId, i, l(c).handleErrorWith(catchObsErrors[F])).pure[F]).some
              case _                                         => none
            }
          }
        )

    private def stopPaused(seqId: Observation.Id): EngineState[F] => Option[Stream[F, EventType[F]]] =
      endPaused(seqId, _.stopPaused)

    private def abortPaused(seqId: Observation.Id): EngineState[F] => Option[Stream[F, EventType[F]]] =
      endPaused(seqId, _.abortPaused)

    def toInstrumentSys(inst: Instrument)(
      implicit ev: Timer[F], cio: Concurrent[F]
    ): F[InstrumentSystem[F]] = inst match {
      case Instrument.F2    => Flamingos2(systems.flamingos2, systems.dhs).pure[F].widen[InstrumentSystem[F]]
      case Instrument.GmosS => GmosSouth(systems.gmosSouth, systems.dhs, gmosNsCmd).pure[F].widen[InstrumentSystem[F]]
      case Instrument.GmosN => GmosNorth(systems.gmosNorth, systems.dhs, gmosNsCmd).pure[F].widen[InstrumentSystem[F]]
      case Instrument.Gnirs => Gnirs(systems.gnirs, systems.dhs).pure[F].widen[InstrumentSystem[F]]
      case Instrument.Gpi   => Gpi(systems.gpi).pure[F].widen[InstrumentSystem[F]]
      case Instrument.Ghost => Ghost(systems.ghost).pure[F].widen[InstrumentSystem[F]]
      case Instrument.Niri  => Niri(systems.niri, systems.dhs).pure[F].widen[InstrumentSystem[F]]
      case Instrument.Nifs  => Nifs(systems.nifs, systems.dhs).pure[F].widen[InstrumentSystem[F]]
      case Instrument.Gsaoi => Gsaoi(systems.gsaoi, systems.dhs).pure[F].widen[InstrumentSystem[F]]
      case _                => Unexpected(s"Instrument $inst not supported.").raiseError[F, InstrumentSystem[F]]
    }

    private def calcResources(sys: List[System[F]]): Set[Resource] = sys.map(_.resource).toSet

    import TcsController.Subsystem._

    private def flatOrArcTcsSubsystems(inst: Instrument): NonEmptySet[TcsController.Subsystem] =
      NonEmptySet.of(AGUnit, (if (inst.hasOI) List(OIWFS) else List.empty): _*)

    private def getTcs(subs: NonEmptySet[TcsController.Subsystem], useGaos: Boolean, inst: InstrumentSystem[F],
                       lsource: LightSource, config: CleanConfig): F[System[F]] = site match {
      case Site.GS => if(useGaos)
        Gems.fromConfig[F](systems.gems, systems.guideDb)(config).map(a =>
          TcsSouth.fromConfig[F](systems.tcsSouth, subs, a.some, inst, systems.guideDb)(
            config, LightPath(lsource, inst.sfName(config)), extractWavelength(config)
          )
        )
        else
          TcsSouth.fromConfig[F](systems.tcsSouth, subs, None, inst, systems.guideDb)(
            config, LightPath(lsource, inst.sfName(config)), extractWavelength(config)
          ).pure[F].widen[System[F]]
      case Site.GN => if(useGaos)
          Altair.fromConfig(config, systems.altair).map(a =>
            TcsNorth.fromConfig[F](systems.tcsNorth, subs, a.some, inst, systems.guideDb)(
              config, LightPath(lsource, inst.sfName(config)), extractWavelength(config)
            )
          )
        else
          TcsNorth.fromConfig[F](systems.tcsNorth, subs, none, inst, systems.guideDb)(
            config, LightPath(lsource, inst.sfName(config)), extractWavelength(config)
          ).pure[F].widen[System[F]]
    }

    private def calcSystems(
      config: CleanConfig,
      stepType: StepType,
      sys: InstrumentSystem[F]
    ): F[List[System[F]]] = {
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

        case StepType.NightFlatOrArc(_)   => for {
          tcs  <- getTcs(NonEmptySet.of(AGUnit, OIWFS, M2, M1, Mount), useGaos = false, sys,
            TcsController.LightSource.GCAL, config
          )
          gcal <- Gcal.fromConfig(systems.gcal, site == Site.GS)(config)
        } yield List(sys, tcs, gcal)

        case StepType.DarkOrBias(_) => List(sys:System[F]).pure[F]

        case StepType.ExclusiveDarkOrBias(_) | StepType.DarkOrBiasNS(_) =>
          List(sys, Gcal.defaultGcal[F](systems.gcal)).pure[F]

        case StepType.AltairObs(inst)        => getTcs(
          inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
          useGaos = true,
          sys,
          TcsController.LightSource.AO,
          config
        ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

        case StepType.AlignAndCalib          => List(sys:System[F]).pure[F]

        case StepType.Gems(inst)             => getTcs(
          inst.hasOI.fold(allButGaos, allButGaosNorOi).add(Gaos),
          useGaos = true,
          sys,
          TcsController.LightSource.AO,
          config
        ).map{ List(sys, _, Gcal.defaultGcal(systems.gcal)) }

        case _                               =>
          Unexpected(s"Unsupported step type $stepType").raiseError[F, List[System[F]]]
      }
    }

    private def calcInstHeader(
      config: CleanConfig,
      sys: InstrumentSystem[F]
    ): F[Header[F]] = {
      sys.resource match {
        case Instrument.F2     =>
          Flamingos2Header.header[F](sys, Flamingos2Header.ObsKeywordsReaderODB(config), systems.tcsKeywordReader).pure[F]
        case Instrument.GmosS |
             Instrument.GmosN  =>
          GmosHeader.header[F](sys, GmosObsKeywordsReader(config), systems.gmosKeywordReader, systems.tcsKeywordReader).pure[F]
        case Instrument.Gnirs  =>
          GnirsHeader.header[F](sys, systems.gnirsKeywordReader, systems.tcsKeywordReader).pure[F]
        case Instrument.Gpi    =>
          GpiHeader.header[F](systems.gpi.gdsClient, systems.tcsKeywordReader, ObsKeywordReader[F](config, site)).pure[F]
        case Instrument.Ghost  =>
          GhostHeader.header[F].pure[F]
        case Instrument.Niri   =>
          NiriHeader.header[F](sys, systems.niriKeywordReader, systems.tcsKeywordReader).pure[F]
        case Instrument.Nifs   =>
          NifsHeader.header[F](sys, systems.nifsKeywordReader, systems.tcsKeywordReader).pure[F]
        case Instrument.Gsaoi   =>
          GsaoiHeader.header[F](sys, systems.tcsKeywordReader, systems.gsaoiKeywordReader).pure[F]
        case _                 =>
          Unexpected(s"Instrument ${sys.resource} not supported.").raiseError[F, Header[F]]
      }
    }

    private def commonHeaders(config: CleanConfig, tcsSubsystems: List[TcsController.Subsystem],
                              inst: InstrumentSystem[F])(ctx: HeaderExtraData): Header[F] =
      new StandardHeader(
        inst,
        ObsKeywordReader[F](config, site),
        systems.tcsKeywordReader,
        StateKeywordsReader[F](ctx.conditions, ctx.operator, ctx.observer),
        tcsSubsystems
      )

    private def gwsHeaders(i: InstrumentSystem[F]): Header[F] = GwsHeader.header(i, systems.gwsKeywordReader)

    private def gcalHeader(i: InstrumentSystem[F]): Header[F] =
      GcalHeader.header(i, systems.gcalKeywordReader)

    private def altairHeader(instrument: InstrumentSystem[F]): Header[F] =
      AltairHeader.header[F](
        instrument,
        systems.altairKeywordReader,
        systems.tcsKeywordReader
      )

    private def altairLgsHeader(guideStar: GuideStarType, instrument: InstrumentSystem[F]): Header[F] =
      if (guideStar === GuideStarType.LGS) {
        AltairLgsHeader.header(instrument, systems.altairKeywordReader)
      } else {
        dummyHeader[F]
      }

    private def gemsHeaders(instrument: InstrumentSystem[F],
                            obsKReader: ObsKeywordsReader[F],
                            tcsKReader: TcsKeywordsReader[F])
    : Header[F] = GemsHeader.header[F](
      instrument,
      systems.gemsKeywordsReader,
      obsKReader,
      tcsKReader
    )

    private def calcHeaders(
      config: CleanConfig,
      stepType: StepType,
      sys: InstrumentSystem[F]
    ): F[HeaderExtraData => List[Header[F]]] = {
      stepType match {
        case StepType.CelestialObject(_) | StepType.NodAndShuffle(_) =>
            calcInstHeader(config, sys).map(h => ctx =>
              List(commonHeaders(config, allButGaos.toList, sys)(ctx), gwsHeaders(sys), h))

        case StepType.AltairObs(_)    =>
          for {
            gst  <- Altair.guideStarType[F](config)
            read <- calcInstHeader(config, sys).map(h => (ctx: HeaderExtraData) =>
                      // Order is important
                      List(
                        commonHeaders(config, allButGaos.toList, sys)(ctx),
                        altairHeader(sys),
                        altairLgsHeader(gst, sys),
                        gwsHeaders(sys), h))
          } yield read

        case StepType.FlatOrArc(inst) =>
            calcInstHeader(config, sys).map(h => ctx =>
              List(commonHeaders(config, flatOrArcTcsSubsystems(inst).toList, sys)(ctx), gcalHeader(sys), gwsHeaders(sys), h))

        case StepType.NightFlatOrArc(_) =>
            calcInstHeader(config, sys).map(h => ctx =>
              List(commonHeaders(config, List(AGUnit, OIWFS, M2, M1, Mount), sys)(ctx), gcalHeader(sys), gwsHeaders(sys), h))

        case StepType.DarkOrBias(_) | StepType.DarkOrBiasNS(_) | StepType.ExclusiveDarkOrBias(_) =>
            calcInstHeader(config, sys).map(h => (ctx => List(commonHeaders(config, Nil, sys)(ctx), gwsHeaders(sys), h)))

        case StepType.AlignAndCalib   => ((_: HeaderExtraData) => List.empty[Header[F]]).pure[F] // No headers for A&C

        case StepType.Gems(_)         =>
          val obsKReader = ObsKeywordReader[F](config, site)
          calcInstHeader(config, sys).map(h => ctx =>
            List(commonHeaders(config, allButGaos.toList, sys)(ctx),
              gwsHeaders(sys),
              gemsHeaders(sys, obsKReader, systems.tcsKeywordReader), h
            )
          )

        case st                       => Unexpected(s"Unsupported step type $st")
          .raiseError[F, HeaderExtraData => List[Header[F]]]
      }
    }

  }

  def apply[F[_]: Sync: Logger](site: Site, systems: Systems[F]): F[SeqTranslate[F]] =
    Ref.of[F, Option[NSObserveCommand]](none).map(new SeqTranslateImpl(site, systems, _))

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
