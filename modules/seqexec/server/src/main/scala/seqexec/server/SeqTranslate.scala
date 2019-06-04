// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.data.{NonEmptySet, Reader}
import cats.effect.{Concurrent, IO, Timer}
import cats.effect.Sync
import cats.effect.LiftIO
import cats.implicits._
import edu.gemini.seqexec.odb.{ExecutedDataset, SeqexecSequence}
import edu.gemini.spModel.ao.AOConstants._
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.gemini.altair.AltairConstants
import edu.gemini.spModel.gemini.altair.AltairParams.GuideStarType
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import fs2.Stream
import gem.Observation
import gem.enum.Site
import mouse.all._
import org.log4s._
import seqexec.engine._
import seqexec.model.enum.{Instrument, Resource}
import seqexec.model.{ActionType, StepState}
import seqexec.model.dhs.ImageFileId
import seqexec.server.ConfigUtilOps._
import seqexec.server.SeqexecFailure.{Unexpected, UnrecognizedInstrument}
import seqexec.server.InstrumentSystem._
import seqexec.server.SequenceGen.StepActionsGen
import seqexec.server.flamingos2.{Flamingos2, Flamingos2Header}
import seqexec.server.keywords._
import seqexec.server.gpi.{Gpi, GpiHeader}
import seqexec.server.ghost.{Ghost, GhostHeader}
import seqexec.server.gsaoi._
import seqexec.server.gcal._
import seqexec.server.gmos.{GmosHeader, GmosObsKeywordsReader, GmosKeywordReaderDummy, GmosKeywordReaderEpics, GmosNorth, GmosSouth}
import seqexec.server.gws.{DummyGwsKeywordsReader, GwsHeader, GwsKeywordsReaderEpics}
import seqexec.server.tcs._
import seqexec.server.tcs.TcsController.LightPath
import seqexec.server.gnirs._
import seqexec.server.niri._
import seqexec.server.nifs._
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairHeader
import seqexec.server.altair.AltairLgsHeader
import seqexec.server.altair.AltairKeywordReaderEpics
import seqexec.server.altair.AltairKeywordReaderDummy
import seqexec.server.SeqexecFailure._
import seqexec.server._
import squants.Time
import squants.time.TimeConversions._

class SeqTranslate(site: Site, systems: Systems[IO], settings: TranslateSettings) {
  private val Log = getLogger

  implicit val show: Show[InstrumentSystem[IO]] = Show.show(_.resource.show)

  import SeqTranslate._

  // All instruments ask the DHS for an ImageFileId
  private def dhsFileId[F[_]: LiftIO](inst: InstrumentSystem[F]): SeqActionF[F, ImageFileId] =
    systems.dhs.createImage(DhsClient.ImageParameters(DhsClient.Permanent, List(inst.contributorName, "dhs-http"))).embed

  private def sendDataStart[F[_]: LiftIO: Monad](obsId: Observation.Id, imageFileId: ImageFileId, dataId: String): SeqActionF[F, Unit] =
    systems.odb.datasetStart(obsId, dataId, imageFileId).toF[F].ifM(
      SeqActionF.void,
      SeqActionF.raiseException(SeqexecFailure.Unexpected("Unable to send DataStart message to ODB."))
    )

  private def sendDataEnd[F[_]: LiftIO: Monad](obsId: Observation.Id, imageFileId: ImageFileId, dataId: String): SeqActionF[F, Unit] =
    systems.odb.datasetComplete(obsId, dataId, imageFileId).toF[F].ifM(
      SeqActionF.void,
      SeqActionF.raiseException(SeqexecFailure.Unexpected("Unable to send DataEnd message to ODB.")))

  private def sendObservationAborted[F[_]: LiftIO: Monad](obsId: Observation.Id, imageFileId: ImageFileId): SeqActionF[F, Unit] =
    systems.odb.obsAbort(obsId, imageFileId).toF[F].ifM(
      SeqActionF.void,
      SeqActionF.raiseException(SeqexecFailure.Unexpected("Unable to send ObservationAborted message to ODB.")))

  private def info[F[_]: Sync](msg: => String): SeqActionF[F, Unit] = SeqActionF.liftF(Sync[F].delay(Log.info(msg)))

  //scalastyle:off
  private def observe(config: Config, obsId: Observation.Id, inst: InstrumentSystem[IO],
                      otherSys: List[System[IO]], headers: Reader[HeaderExtraData, List[Header[IO]]])
                     (ctx: HeaderExtraData)
                     (implicit ev: Concurrent[IO]): Stream[IO, Result]
  = {
    def dataId[F[_]: Sync]: SeqActionF[F, String] = SeqActionF.either(
      config.extractAs[String](OBSERVE_KEY / DATA_LABEL_PROP).leftMap(e =>
      SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))))

    def notifyObserveStart[F[_]: Sync: LiftIO]: SeqActionF[F, Unit] = otherSys.map(_.notifyObserveStart).sequence.void.toF[F]

    // endObserve must be sent to the instrument too.
    def notifyObserveEnd[F[_]: Sync: LiftIO]: SeqActionF[F, Unit] = (inst +: otherSys).map(_.notifyObserveEnd).sequence.void.toF[F]

    def closeImage[F[_]: LiftIO](id: ImageFileId): SeqActionF[F, Unit] =
      SeqActionF.embed(inst.keywordsClient.closeImage(id))

    def doObserve[F[_]](fileId: ImageFileId): SeqAction[Result] =
      for {
        d   <- dataId
        _   <- sendDataStart(obsId, fileId, d)
        _   <- notifyObserveStart
        _   <- headers(ctx).map(_.sendBefore(obsId, fileId)).sequence.embed
        _   <- info(s"Start ${inst.resource.show} observation ${obsId.format} with label $fileId")
        r   <- inst.observe(config)(fileId)
        _   <- info(s"Completed ${inst.resource.show} observation ${obsId.format} with label $fileId")
        ret <- observeTail(fileId, d)(r)
      } yield ret

    def observeTail(id: ImageFileId, dataId: String)(r: ObserveCommand.Result): SeqActionF[IO, Result] = {
      def okTail[F[_]: Sync: LiftIO](stopped: Boolean): SeqActionF[F, Result] = for {
        _ <- notifyObserveEnd[F]
        _ <- SeqActionF.embed[F, Unit](headers(ctx).reverseMap(_.sendAfter(id)).sequence.void)
        _ <- closeImage[F](id)
        _ <- sendDataEnd[F](obsId, id, dataId)
      } yield if (stopped) Result.OKStopped(Response.Observed(id)) else Result.OK(Response.Observed(id))

      val successTail: SeqAction[Result] = okTail(stopped = false)

      val stopTail: SeqAction[Result] = okTail(stopped = true)

      val abortTail: SeqAction[Result] = sendObservationAborted(obsId, id) *>
        SeqAction.fail(SeqexecFailure.Execution(s"Observation ${obsId.format} aborted by user."))

      r match {
        case ObserveCommand.Success => successTail
        case ObserveCommand.Stopped => stopTail
        case ObserveCommand.Aborted => abortTail
        case ObserveCommand.Paused  =>
          SeqActionF.liftF(inst.calcObserveTime(config))
            .map(e => Result.Paused(ObserveContext(observeTail(id, dataId), e)))
      }
    }

    Stream.eval(dhsFileId(inst).value).flatMap {
      case Right(id) =>
        val observationProgressStream =
          for {
            ot <- Stream.eval(inst.calcObserveTime(config))
            pr <- inst.observeProgress(ot, ElapsedTime(0.0.seconds))
          } yield Result.Partial(pr)

        val observationCommand =
          Stream.eval[IO, Result](doObserve(id).value.map(_.toResult))

        Stream.emit(Result.Partial(FileIdAllocated(id))).covary[IO] ++
          observationProgressStream.mergeHaltR(observationCommand)
      case Left(e)   => Stream.emit(Result.Error(SeqexecFailure.explain(e))).covary[IO]
    }
  }

  private def step(obsId: Observation.Id, i: Int, config: Config, nextToRun: Int,
                   datasets: Map[Int, ExecutedDataset])(
                     implicit cio: Concurrent[IO],
                              tio: Timer[IO]
                   ): TrySeq[SequenceGen.StepGen[IO]] = {
    def buildStep(
      inst: InstrumentSystem[IO],
      sys: List[System[IO]],
      headers: Reader[HeaderExtraData, List[Header[IO]]],
      stepType: StepType
    ): SequenceGen.StepGen[IO] = {
      val initialStepExecutions: List[List[Action[IO]]] =
        (i === 0 && stepType.includesObserve).option {
          List(List(systems.odb.sequenceStart(obsId, "")
            .as(Response.Ignored).toAction(ActionType.Undefined)))
        }.orEmpty

      val configs: Map[Resource, Action[IO]] = sys.map { x =>
        val res = resourceFromSystem(x)
        val kind = ActionType.Configure(res)

        res -> x.configure(config).as(Response.Configured(x.resource)).toAction(kind)
      }.toMap

      def rest(ctx: HeaderExtraData): List[List[Action[IO]]] =
        (stepType.includesObserve).option {
          List(
            List(Action(ActionType.Observe, observe(config, obsId, inst, sys.filterNot(inst.equals),
              headers)(ctx), Action.State(Action.Idle, Nil)))
          )
        }.orEmpty

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
          datasets.get(i + 1).map(_.filename)
        )
      }
    }

    for {
      stepType  <- calcStepType(config)
      inst      <- toInstrumentSys(stepType.instrument)
      systems   <- calcSystems(config, stepType, inst)
      headers   <- calcHeaders(config, stepType, inst)
    } yield buildStep(inst, systems, headers, stepType)
  }

  // Required for untyped objects from java
  implicit val objectShow: Show[AnyRef] = Show.fromToString

  private def extractStatus(config: Config): StepState =
    config.getItemValue(new ItemKey("observe:status")).show match {
      case "ready"    => StepState.Pending
      case "complete" => StepState.Completed
      case "skipped"  => StepState.Skipped
      case kw         => StepState.Failed("Unexpected status keyword: " ++ kw)
    }

  def sequence(obsId: Observation.Id, sequence: SeqexecSequence)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): (List[SeqexecFailure], Option[SequenceGen[IO]]) = {

    val configs = sequence.config.getAllSteps.toList

    val nextToRun = configs.map(extractStatus).lastIndexWhere(s => s === StepState.Completed || s === StepState.Skipped) + 1

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

  private def deliverObserveCmd(seqId: Observation.Id, f: ObserveControl[IO] => Option[SeqAction[Unit]])(st: EngineState)(
    implicit tio: Timer[IO]
  ):  Option[Stream[IO, executeEngine.EventType]] = {
    def isObserving(v: Action[IO]): Boolean = v.kind === ActionType.Observe && (v.state.runState match {
      case Action.Started               => true
      case _                            => false
    })

    def seqCmd(seqState: Sequence.State[IO], instrument: Instrument): Option[Stream[IO, executeEngine.EventType]] =
      toInstrumentSys(instrument).toOption.flatMap(x => f(x.observeControl)).flatMap {
        v => seqState.current.execution.exists(isObserving).option(Stream.eval(v.value.map(handleError)))
      }

    for {
      seqg   <- st.sequences.seq.get(seqId)
      obsseq <- st.sequences.get(seqId)
      r      <- seqCmd(obsseq.seq, seqg.seqGen.instrument)
    } yield r

  }

  private def handleError(t: TrySeq[Unit]): executeEngine.EventType = t match {
    case Left(e) => Event.logErrorMsg(SeqexecFailure.explain(e))
    case _       => Event.nullEvent
  }

  def stopObserve(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = st =>{
    def f(oc: ObserveControl[IO]): Option[SeqAction[Unit]] = oc match {
      case CompleteControl(StopObserveCmd(stop), _, _, _, _, _) => stop.some
      case UnpausableControl(StopObserveCmd(stop), _)           => stop.some
      case _                                                    => none
    }
    deliverObserveCmd(seqId, f)(st).orElse(stopPaused(seqId).apply(st))
  }

  def abortObserve(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = st => {
    def f(oc: ObserveControl[IO]): Option[SeqAction[Unit]] = oc match {
      case CompleteControl(_, AbortObserveCmd(abort), _, _, _, _) => abort.some
      case UnpausableControl(_, AbortObserveCmd(abort))           => abort.some
      case _                                                      => none
    }

    deliverObserveCmd(seqId, f)(st).orElse(abortPaused(seqId).apply(st))
  }

  def pauseObserve(seqId: Observation.Id)(
    implicit tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(oc: ObserveControl[IO]): Option[SeqAction[Unit]] = oc match {
      case CompleteControl(_, _, PauseObserveCmd(pause), _, _, _) => pause.some
      case _                                                      => none
    }
    deliverObserveCmd(seqId, f)
  }

  private def pausedCommand(seqId: Observation.Id,
                            f: ObserveControl[IO] => Option[Time => SeqAction[ObserveCommand.Result]],
                            useCountdown: Boolean)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO,executeEngine.EventType]] = st => {

    def resumeIO(c: ObserveContext, resumeCmd: SeqAction[ObserveCommand.Result]): IO[Result] = (for {
      r <- resumeCmd
      ret <- c.t(r)
    } yield ret).value.map(_.toResult)

    def seqCmd(seqState: Sequence.State[IO], instrument: Instrument): Option[Stream[IO,
      executeEngine.EventType]] = {

      val inst = toInstrumentSys(instrument).toOption

      val observeIndex: Option[(ObserveContext, Option[Time], Int)] =
        seqState.current.execution.zipWithIndex.find(_._1.kind === ActionType.Observe).flatMap {
          case (a, i) => a.state.runState match {
            case Action.Paused(c: ObserveContext) => Some((c, a.state.partials.collectFirst{
              case x@Progress(_, _) => x.progress}, i))
            case _ => none
          }
        }

      val u: Option[Time => SeqAction[ObserveCommand.Result]] =
        inst.flatMap(x => f(x.observeControl))

      (u, observeIndex, inst).mapN {
        (cmd, t, ins) =>
          t match {
            case (c, to, i) =>
              if(useCountdown)
                Stream.eval(IO(Event.actionResume(seqId, i,
                  ins.observeProgress(c.expTime, ElapsedTime(to.getOrElse(0.0.seconds)))
                    .map(Result.Partial(_))
                    .mergeHaltR(Stream.eval(resumeIO(c, cmd(c.expTime))))
                )))
              else
                Stream.eval(IO(Event.actionResume(seqId, i,
                  Stream.eval(resumeIO(c, cmd(c.expTime))))))
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
    def f(o: ObserveControl[IO]): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case CompleteControl(_, _, _, ContinuePausedCmd(a), _, _) => a.some
      case _                                                    => none
    }

    pausedCommand(seqId, f, useCountdown = true)
  }

  private def stopPaused(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl[IO]): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case CompleteControl(_, _, _, _, StopPausedCmd(a), _) => Some(_ => a)
      case _                                                => none
    }

    pausedCommand(seqId, f, useCountdown = false)
  }

  private def abortPaused(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl[IO]): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case CompleteControl(_, _, _, _, _, AbortPausedCmd(a)) => Some(_ => a)
      case _                                                 => none
    }

    pausedCommand(seqId, f, useCountdown = false)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private def toInstrumentSys(inst: Instrument)(
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
    sys.map(resourceFromSystem[F]).toSet

  import TcsController.Subsystem._

  private def hasOI(inst: Instrument): Boolean = inst match {
    case Instrument.F2    => true
    case Instrument.GmosS => true
    case Instrument.GmosN => true
    case Instrument.Nifs  => true
    case Instrument.Niri  => true
    case Instrument.Gsaoi => false
    case Instrument.Gpi   => true
    case Instrument.Ghost => false
    case _                => false
  }

  private def flatOrArcTcsSubsystems(inst: Instrument): NonEmptySet[TcsController.Subsystem] =
    NonEmptySet.of(AGUnit, (if (hasOI(inst)) List(OIWFS) else List.empty): _*)

  private def extractWavelength(config: Config): Option[Wavelength] =
    config.extractAs[Wavelength](OBSERVING_WAVELENGTH_KEY).toOption

  private def calcSystems(
    config: Config,
    stepType: StepType,
    sys: InstrumentSystem[IO]
  ): TrySeq[List[System[IO]]] = {
    stepType match {
      case CelestialObject(inst) =>  (sys :: List(
          Tcs.fromConfig(systems.tcs, hasOI(inst).fold(allButGaos, allButGaosNorOi), None, sys, systems.guideDb)(
            config,
            LightPath(TcsController.LightSource.Sky, sys.sfName(config)),
            extractWavelength(config)),
          Gcal(systems.gcal, site == Site.GS)
      )).asRight

      case FlatOrArc(inst)       =>  (sys :: List(
          Tcs.fromConfig(systems.tcs, flatOrArcTcsSubsystems(inst), None, sys, systems.guideDb)(
            config,
            LightPath(TcsController.LightSource.GCAL, sys.sfName(config)),
            extractWavelength(config)),
          Gcal(systems.gcal, site == Site.GS)
      )).asRight

      case DarkOrBias(_)      =>  List(sys).asRight

      case AltairObs(inst)          =>
        Altair.fromConfig(config, systems.altair).map {altair =>
          sys :: List(
            Tcs.fromConfig(systems.tcs, hasOI(inst).fold(allButGaos, allButGaosNorOi).add(Gaos),
              altair.asLeft.some, sys, systems.guideDb)(config,
              LightPath(TcsController.LightSource.AO, sys.sfName(config)),
              extractWavelength(config)),
            Gcal(systems.gcal, site == Site.GS)
        )}

      case AlignAndCalib         => List(sys).asRight

      case _                     => TrySeq.fail(Unexpected(s"Unsupported step type $stepType"))
    }
  }

  // I cannot use a sealed trait as base, because I cannot have all systems in one source file (too big),
  // so either I use an unchecked notation, or add a default case that throws an exception.
  private def resourceFromSystem[F[_]](s: System[F]): Resource = (s: @unchecked) match {
    case Tcs(_, _, _, _)  => Resource.TCS
    case Gcal(_, _)       => Resource.Gcal
    case GmosNorth(_, _)  => Instrument.GmosN
    case GmosSouth(_, _)  => Instrument.GmosS
    case Flamingos2(_, _) => Instrument.F2
    case Gnirs(_, _)      => Instrument.Gnirs
    case Gpi(_)           => Instrument.Gpi
    case Gsaoi(_, _)      => Instrument.Gsaoi
    case Ghost(_)         => Instrument.Ghost
    case Niri(_, _)       => Instrument.Niri
    case Nifs(_, _)       => Instrument.Nifs
  }

  private def calcInstHeader(
    config: Config,
    sys: InstrumentSystem[IO]
  ): TrySeq[Header[IO]] = {
    val tcsKReader = if (settings.tcsKeywords) TcsKeywordsReaderEpics[IO] else DummyTcsKeywordsReader[IO]
    sys.resource match {
      case Instrument.F2     =>
        Flamingos2Header.header[IO](sys, Flamingos2Header.ObsKeywordsReaderODB(config), tcsKReader).asRight
      case Instrument.GmosS |
           Instrument.GmosN  =>
        val gmosInstReader = if (settings.gmosKeywords) GmosKeywordReaderEpics[IO] else GmosKeywordReaderDummy[IO]
        GmosHeader.header[IO](sys, GmosObsKeywordsReader(config), gmosInstReader, tcsKReader).asRight
      case Instrument.Gnirs  =>
        val gnirsReader = if(settings.gnirsKeywords) GnirsKeywordReaderEpics[IO] else GnirsKeywordReaderDummy[IO]
        GnirsHeader.header[IO](sys, gnirsReader, tcsKReader).asRight
      case Instrument.Gpi    =>
        GpiHeader.header[IO](systems.gpi.gdsClient, tcsKReader, ObsKeywordReaderImpl[IO](config, site)).asRight
      case Instrument.Ghost  =>
        GhostHeader.header[IO].asRight
      case Instrument.Niri   =>
        val niriReader = if(settings.niriKeywords) NiriKeywordReaderEpics[IO]
                          else NiriKeywordReaderDummy[IO]
        NiriHeader.header[IO](sys, niriReader, tcsKReader).asRight
      case Instrument.Nifs   =>
        val nifsReader = if(settings.nifsKeywords) NifsKeywordReaderEpics[IO] else NifsKeywordReaderDummy[IO]
        NifsHeader.header[IO](sys, nifsReader, tcsKReader).asRight
      case Instrument.Gsaoi   =>
        val gsaoiReader = if (settings.gsaoiKeywords) GsaoiKeywordReaderEpics[IO](GsaoiEpics.instance) else GsaoiKeywordReaderDummy[IO]
        GsaoiHeader.header[IO](sys, tcsKReader, gsaoiReader).asRight
      case _                 =>
        TrySeq.fail(Unexpected(s"Instrument ${sys.resource} not supported."))
    }
  }

  private def commonHeaders[F[_]: Sync: LiftIO](config: Config, tcsSubsystems: List[TcsController.Subsystem],
                            inst: InstrumentSystem[F])(ctx: HeaderExtraData): Header[F] =
    new StandardHeader(
      inst,
      ObsKeywordReaderImpl(config, site),
      if (settings.tcsKeywords) TcsKeywordsReaderEpics[F] else DummyTcsKeywordsReader[F],
      StateKeywordsReader[F](ctx.conditions, ctx.operator, ctx.observer),
      tcsSubsystems
    )

  private def gwsHeaders[F[_]: Sync: LiftIO](i: InstrumentSystem[F]): Header[F] = GwsHeader.header(i,
    if (settings.gwsKeywords) GwsKeywordsReaderEpics[F] else DummyGwsKeywordsReader[F])

  private def gcalHeader[F[_]: Sync: LiftIO](i: InstrumentSystem[F]): Header[F] = GcalHeader.header(i,
    if (settings.gcalKeywords) GcalKeywordsReaderEpics[F] else DummyGcalKeywordsReader[F] )

  private def altairHeader[F[_]: Sync: LiftIO](instrument: InstrumentSystem[F], tcsKReader: TcsKeywordsReader[F]): Header[F] =
    AltairHeader.header[F](
      instrument,
      if (settings.altairKeywords) AltairKeywordReaderEpics[F] else AltairKeywordReaderDummy[F],
      tcsKReader)

  private def altairLgsHeader[F[_]: Sync: LiftIO](guideStar: GuideStarType, instrument: InstrumentSystem[F]): Header[F] =
    if (guideStar === GuideStarType.LGS) {
      AltairLgsHeader.header(instrument,
        if (settings.altairKeywords) AltairKeywordReaderEpics[F] else AltairKeywordReaderDummy[F])
    } else {
      dummyHeader[F]
    }

  private def calcHeaders(
    config: Config,
    stepType: StepType,
    sys: InstrumentSystem[IO]
  ): TrySeq[Reader[HeaderExtraData, List[Header[IO]]]] = stepType match {
    case CelestialObject(_) =>
        calcInstHeader(config, sys).map(h => Reader(ctx =>
          List(commonHeaders(config, allButGaos.toList, sys)(ctx), gwsHeaders(sys), h)))

    case AltairObs(_) =>
      val tcsKReader = if (settings.tcsKeywords) TcsKeywordsReaderEpics[IO] else DummyTcsKeywordsReader[IO]
      for {
        gst  <- Altair.guideStarType(config)
        read <- calcInstHeader(config, sys).map(h => Reader((ctx: HeaderExtraData) =>
                  // Order is important
                  List(
                    commonHeaders(config, allButGaos.toList, sys)(ctx),
                    altairHeader(sys, tcsKReader),
                    altairLgsHeader(gst, sys),
                    gwsHeaders(sys), h)))
      } yield read

    case FlatOrArc(inst)       =>
        calcInstHeader(config, sys).map(h => Reader(ctx =>
          List(commonHeaders(config, flatOrArcTcsSubsystems(inst).toList, sys)(ctx), gcalHeader(sys), gwsHeaders(sys), h)))

    case DarkOrBias(_)      =>
        calcInstHeader(config, sys).map(h => Reader(ctx => List(commonHeaders(config, Nil, sys)(ctx), gwsHeaders(sys), h)))
    case AlignAndCalib         => TrySeq(Reader(_ => Nil)) // No headers for A&C
    case st                    => TrySeq.fail(Unexpected(s"Unsupported step type $st"))
  }

}

object SeqTranslate {
  def apply(site: Site, systems: Systems[IO], settings: TranslateSettings): SeqTranslate =
    new SeqTranslate(site, systems, settings)

  private def extractInstrument(config: Config): TrySeq[Instrument] = {
    config.extractAs[String](INSTRUMENT_KEY / INSTRUMENT_NAME_PROP).asTrySeq.flatMap {
      case Flamingos2.name => TrySeq(Instrument.F2)
      case GmosSouth.name  => TrySeq(Instrument.GmosS)
      case GmosNorth.name  => TrySeq(Instrument.GmosN)
      case Gnirs.name      => TrySeq(Instrument.Gnirs)
      case Gpi.name        => TrySeq(Instrument.Gpi)
      case Ghost.name      => TrySeq(Instrument.Ghost)
      case Niri.name       => TrySeq(Instrument.Niri)
      case Nifs.name       => TrySeq(Instrument.Nifs)
      case Gsaoi.name      => TrySeq(Instrument.Gsaoi)
      case ins             => TrySeq.fail(UnrecognizedInstrument(s"inst $ins"))
    }
  }

  def isAlignAndCalib(config: Config): Option[StepType] =
    Gpi.isAlignAndCalib(config).option(AlignAndCalib)

  private def calcStepType(config: Config): TrySeq[StepType] = {
    def extractGaos(inst: Instrument): TrySeq[StepType] = config.extractAs[String](AO_SYSTEM_KEY) match {
      case Left(ConfigUtilOps.ConversionError(_, _))              => TrySeq.fail(Unexpected("Unable to get AO system from sequence"))
      case Left(ConfigUtilOps.ContentError(_))                    => TrySeq.fail(Unexpected("Logical error"))
      case Left(ConfigUtilOps.KeyNotFound(_))                     => TrySeq(CelestialObject(inst))
      case Right(AltairConstants.SYSTEM_NAME_PROP)                => TrySeq(AltairObs(inst))
      case Right(edu.gemini.spModel.gemini.gems.Gems.SYSTEM_NAME) => TrySeq(Gems(inst))
      case _                                                      => TrySeq.fail(Unexpected("Logical error reading AO system name"))
    }

    isAlignAndCalib(config).map(_.asRight).getOrElse {
      (config.extractAs[String](OBSERVE_KEY / OBSERVE_TYPE_PROP).leftMap(explainExtractError), extractInstrument(config)).mapN { (obsType, inst) =>
        obsType match {
          case SCIENCE_OBSERVE_TYPE                     => extractGaos(inst)
          case BIAS_OBSERVE_TYPE | DARK_OBSERVE_TYPE    => TrySeq(DarkOrBias(inst))
          case FLAT_OBSERVE_TYPE | ARC_OBSERVE_TYPE | CAL_OBSERVE_TYPE
                                                        => TrySeq(FlatOrArc(inst))
          case _                                        => TrySeq.fail(Unexpected("Unknown step type " + obsType))
        }
      }.flatten
    }
  }

  implicit class ResponseToResult(val r: Either[SeqexecFailure, Response]) extends AnyVal {
    def toResult: Result = r.fold(e => Result.Error(SeqexecFailure.explain(e)), r => Result.OK(r))
  }

  implicit class ResultToResult(val r: Either[SeqexecFailure, Result]) extends AnyVal {
    def toResult: Result = r.fold(e => Result.Error(SeqexecFailure.explain(e)), identity)
  }

  implicit class ConfigResultToResult[F[_], A <: Result.PartialVal](val r: Either[SeqexecFailure, ConfigResult[F]]) extends AnyVal {
    def toResult: Result = r.fold(e => Result.Error(SeqexecFailure.explain(e)), r => Result.OK(
      Response.Configured(r.sys.resource)))
  }

  implicit class ActionResponseToAction[F[_]: Functor, A <: Response](val x: SeqActionF[F, A]) {
    def toAction(kind: ActionType): Action[F] = fromF[F](kind, x.value.map(_.toResult))
  }

  implicit class ConfigResultToAction[F[_]: Functor](val x: SeqActionF[F, ConfigResult[F]]) {
    def toAction(kind: ActionType): Action[F] = fromF[F](kind, x.value.map(_.toResult))
  }

  final case class ObserveContext(t: ObserveCommand.Result => SeqAction[Result], expTime: Time) extends Result.PauseContext
}
