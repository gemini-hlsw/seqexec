// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.data.{EitherT, NonEmptySet, Reader}
import cats.effect.{Concurrent, IO, Timer}
import cats.effect.LiftIO
import cats.implicits._
import edu.gemini.seqexec.odb.{ExecutedDataset, SeqexecSequence}
import edu.gemini.spModel.ao.AOConstants._
import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.gemini.altair.AltairConstants
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
import seqexec.server.gcal._
import seqexec.server.gmos.{GmosHeader, GmosNorth, GmosSouth}
import seqexec.server.gws.{DummyGwsKeywordsReader, GwsHeader, GwsKeywordsReaderImpl}
import seqexec.server.tcs._
import seqexec.server.tcs.TcsController.ScienceFoldPosition
import seqexec.server.gnirs._
import seqexec.server.niri._
import seqexec.server.nifs._
import seqexec.server.altair.Altair
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

  //scalastyle:off
  private def observe(config: Config, obsId: Observation.Id, inst: InstrumentSystem[IO],
                      otherSys: List[System[IO]], headers: Reader[HeaderExtraData, List[Header[IO]]])
                     (ctx: HeaderExtraData)
                     (implicit ev: Concurrent[IO]): Stream[IO, Result]
  = {
    val dataId: SeqAction[String] = SeqAction.either(
      config.extractAs[String](OBSERVE_KEY / DATA_LABEL_PROP).leftMap(e =>
      SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))))

    def notifyObserveStart: SeqAction[Unit] = otherSys.map(_.notifyObserveStart).sequence.void

    // endObserve must be sent to the instrument too.
    def notifyObserveEnd: SeqAction[Unit] = (inst +: otherSys).map(_.notifyObserveEnd).sequence.void

    def closeImage[F[_]: LiftIO](id: ImageFileId): SeqActionF[F, Unit] =
      SeqActionF.embed(inst.keywordsClient.closeImage(id))

    def doObserve(fileId: ImageFileId): SeqAction[Result] =
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

    def observeTail(id: ImageFileId, dataId: String)(r: ObserveCommand.Result): SeqAction[Result] = {
      val successTail: SeqAction[Result] = for {
        _ <- notifyObserveEnd
        _ <- headers(ctx).reverseMap(_.sendAfter(id)).sequence.embed
        _ <- closeImage(id)
        _ <- sendDataEnd(obsId, id, dataId)
      } yield Result.OK(Response.Observed(id))

      val stopTail: SeqAction[Result] = successTail
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
                   ): TrySeq[SequenceGen.StepGen] = {
    def buildStep(inst: InstrumentSystem[IO], sys: List[System[IO]],
                  headers: Reader[HeaderExtraData, List[Header[IO]]]): SequenceGen.StepGen = {
      val initialStepExecutions: List[List[Action[IO]]] =
        if (i === 0)
          List(List(systems.odb.sequenceStart(obsId, "")
            .as(Response.Ignored).toAction(ActionType.Undefined)))
        else Nil

      val configs: Map[Resource, Action[IO]] = sys.map { x =>
        val res = resourceFromSystem(x)
        val kind = ActionType.Configure(res)

        res -> x.configure(config).as(Response.Configured(x.resource)).toAction(kind)
      }.toMap
      def rest(ctx:HeaderExtraData): List[List[Action[IO]]] = List(
        List(Action(ActionType.Observe, observe(config, obsId, inst, sys.filterNot(inst.equals),
          headers)(ctx), Action.State(Action.Idle, Nil)))
      )

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
      systems   <- calcSystems(config, stepType)
      headers   <- calcHeaders(config, stepType)
    } yield buildStep(inst, systems, headers)
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

  def sequence(obsId: Observation.Id, sequence: SeqexecSequence)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ):
      (List[SeqexecFailure], Option[SequenceGen]) = {

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
              SequenceGen(
                obsId,
                sequence.title,
                i,
                ss
              )
            )
        )
      })
  }

  private def deliverObserveCmd(seqId: Observation.Id, f: ObserveControl => Option[SeqAction[Unit]])(st: EngineState)(
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
    def f(oc: ObserveControl): Option[SeqAction[Unit]] = oc match {
      case OpticControl(StopObserveCmd(stop), _, _, _, _, _) => Some(stop)
      case InfraredControl(StopObserveCmd(stop), _)          => Some(stop)
      case _                                                 => none
    }
    deliverObserveCmd(seqId, f)(st).orElse(stopPaused(seqId).apply(st))
  }

  def abortObserve(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = st => {
    def f(oc: ObserveControl): Option[SeqAction[Unit]] = oc match {
      case OpticControl(_, AbortObserveCmd(abort), _, _, _, _) => Some(abort)
      case InfraredControl(_, AbortObserveCmd(abort))          => Some(abort)
      case _                                                   => none
    }

    deliverObserveCmd(seqId, f)(st).orElse(abortPaused(seqId).apply(st))
  }

  def pauseObserve(seqId: Observation.Id)(
    implicit tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(oc: ObserveControl): Option[SeqAction[Unit]] = oc match {
      case OpticControl(_, _, PauseObserveCmd(pause), _, _, _) => Some(pause)
      case _                                                   => none
    }
    deliverObserveCmd(seqId, f)
  }

  private def pausedCommand(seqId: Observation.Id,
                            f: ObserveControl => Option[Time => SeqAction[ObserveCommand.Result]],
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
    def f(o: ObserveControl): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case OpticControl(_, _, _, ContinuePausedCmd(a), _, _) => Some(a)
      case _                                                 => none
    }

    pausedCommand(seqId, f, useCountdown = true)
  }

  private def stopPaused(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case OpticControl(_, _, _, _, StopPausedCmd(a), _) => Some(_ => a)
      case _                                             => none
    }

    pausedCommand(seqId, f, useCountdown = false)
  }

  private def abortPaused(seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): EngineState => Option[Stream[IO, executeEngine.EventType]] = {
    def f(o: ObserveControl): Option[Time => SeqAction[ObserveCommand.Result]] = o match {
      case OpticControl(_, _, _, _, _, AbortPausedCmd(a)) => Some(_ => a)
      case _                                              => none
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
    case _                => TrySeq.fail(Unexpected(s"Instrument $inst not supported."))
  }

  private def calcResources(sys: List[System[IO]]): Set[Resource] =
    sys.map(resourceFromSystem).toSet

  import TcsController.Subsystem._

  private def hasOI(inst: Instrument): Boolean = inst match {
    case Instrument.F2    => true
    case Instrument.GmosS => true
    case Instrument.GmosN => true
    case Instrument.Nifs  => true
    case Instrument.Niri  => true
    case Instrument.Gpi   => true
    case Instrument.Ghost => false
    case _                => false
  }

  private def flatOrArcTcsSubsystems(inst: Instrument): NonEmptySet[TcsController.Subsystem] =
    NonEmptySet.of(AGUnit, (if (hasOI(inst)) List(OIWFS) else List.empty): _*)

  private def extractWavelength(config: Config): Option[Wavelength] =
    config.extractAs[Wavelength](OBSERVING_WAVELENGTH_KEY).toOption

  private def calcSystems(config: Config, stepType: StepType)(
    implicit tio: Timer[IO]
  ): TrySeq[List[System[IO]]] = {
    stepType match {
      case CelestialObject(inst) => toInstrumentSys(inst).map{sys => sys :: List(
          Tcs.fromConfig(systems.tcs, (hasOI(inst)).fold(allButGaos, allButGaosNorOi), None, systems.guideDb)(
            config,
            ScienceFoldPosition.Position(TcsController.LightSource.Sky, sys.sfName(config)),
            extractWavelength(config)),
          Gcal(systems.gcal, site == Site.GS)
      ) }
      case FlatOrArc(inst)       => toInstrumentSys(inst).map{sys => sys :: List(
          Tcs.fromConfig(systems.tcs, flatOrArcTcsSubsystems(inst), None, systems.guideDb)(
            config,
            ScienceFoldPosition.Position(TcsController.LightSource.GCAL, sys.sfName(config)),
            extractWavelength(config)),
          Gcal(systems.gcal, site == Site.GS)
      ) }
      case DarkOrBias(inst)      => toInstrumentSys(inst).map(List(_))
      case AltairObs(inst)          => for{
        sys    <- toInstrumentSys(inst)
        altair <- Altair.fromConfig(config, systems.altair)
      } yield sys :: List(
          Tcs.fromConfig(systems.tcs, hasOI(inst).fold(allButGaos, allButGaosNorOi).add(Gaos),
            altair.asLeft.some, systems.guideDb)(config,
            ScienceFoldPosition.Position(TcsController.LightSource.Sky, sys.sfName(config)),
            extractWavelength(config)),
          Gcal(systems.gcal, site == Site.GS)


      )
      case _                     => TrySeq.fail(Unexpected(s"Unsupported step type $stepType"))
    }
  }

  // I cannot use a sealed trait as base, because I cannot have all systems in one source file (too big),
  // so either I use an unchecked notation, or add a default case that throws an exception.
  private def resourceFromSystem(s: System[IO]): Resource = (s: @unchecked) match {
    case Tcs(_, _, _, _)  => Resource.TCS
    case Gcal(_, _)       => Resource.Gcal
    case GmosNorth(_, _)  => Instrument.GmosN
    case GmosSouth(_, _)  => Instrument.GmosS
    case Flamingos2(_, _) => Instrument.F2
    case Gnirs(_, _)      => Instrument.Gnirs
    case Gpi(_)           => Instrument.Gpi
    case Ghost(_)         => Instrument.Ghost
    case Niri(_, _)       => Instrument.Niri
    case Nifs(_, _)       => Instrument.Nifs
  }

  private def calcInstHeader(config: Config, inst: Instrument)(
    implicit tio: Timer[IO]
  ): TrySeq[Header[IO]] = {
    val tcsKReader = if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader
    inst match {
      case Instrument.F2     =>
        toInstrumentSys(inst).map(Flamingos2Header.header(_, new Flamingos2Header.ObsKeywordsReaderImpl(config), tcsKReader))
      case Instrument.GmosS |
           Instrument.GmosN  =>
        val gmosInstReader = if (settings.gmosKeywords) GmosHeader.InstKeywordReaderImpl else GmosHeader.DummyInstKeywordReader
        toInstrumentSys(inst).map(GmosHeader.header(_, GmosHeader.ObsKeywordsReaderImpl(config), gmosInstReader, tcsKReader))
      case Instrument.Gnirs  =>
        val gnirsReader = if(settings.gnirsKeywords) GnirsKeywordReaderImpl else GnirsKeywordReaderDummy
        toInstrumentSys(inst).map(GnirsHeader.header(_, gnirsReader, tcsKReader))
      case Instrument.Gpi    =>
        TrySeq(GpiHeader.header(systems.gpi.gdsClient, tcsKReader, ObsKeywordReaderImpl[IO](config, site)))
      case Instrument.Ghost  =>
        GhostHeader.header[IO].asRight
      case Instrument.Niri   =>
        val niriReader = if(settings.niriKeywords) NiriKeywordReaderImpl
                          else NiriKeywordReaderDummy
        toInstrumentSys(inst).map(NiriHeader.header(_, niriReader, tcsKReader))
      case Instrument.Nifs   =>
        val nifsReader = if(settings.nifsKeywords) NifsKeywordReaderImpl else NifsKeywordReaderDummy
        toInstrumentSys(inst).map(NifsHeader.header(_, nifsReader, tcsKReader))
      case _                 =>
        TrySeq.fail(Unexpected(s"Instrument $inst not supported."))
    }
  }

  private def commonHeaders(config: Config, tcsSubsystems: List[TcsController.Subsystem],
                            inst: InstrumentSystem[IO])(ctx: HeaderExtraData): Header[IO] =
    new StandardHeader(
      inst,
      ObsKeywordReaderImpl(config, site),
      if (settings.tcsKeywords) TcsKeywordsReaderImpl else DummyTcsKeywordsReader,
      StateKeywordsReader[IO](ctx.conditions, ctx.operator, ctx.observer),
      tcsSubsystems
    )

  private def gwsHeaders(i: InstrumentSystem[IO]): Header[IO] = GwsHeader.header(i,
    if (settings.gwsKeywords) GwsKeywordsReaderImpl else DummyGwsKeywordsReader)

  private def gcalHeader(i: InstrumentSystem[IO]): Header[IO] = GcalHeader.header(i,
    if (settings.gcalKeywords) GcalKeywordsReaderImpl else DummyGcalKeywordsReader )

  private def calcHeaders(config: Config, stepType: StepType)(
    implicit tio: Timer[IO]
  ): TrySeq[Reader[HeaderExtraData, List[Header[IO]]]] = stepType match {
    case CelestialObject(inst) => toInstrumentSys(inst) >>= { i =>
        calcInstHeader(config, inst).map(h => Reader(ctx =>
          List(commonHeaders(config, allButGaos.toList, i)(ctx), gwsHeaders(i), h)))
      }
    case AltairObs(inst) => toInstrumentSys(inst) >>= { i =>
        calcInstHeader(config, inst).map(h => Reader(ctx =>
          List(commonHeaders(config, allButGaos.toList, i)(ctx), gwsHeaders(i), h)))
      }
    case FlatOrArc(inst)       => toInstrumentSys(inst) >>= { i =>
        calcInstHeader(config, inst).map(h => Reader(ctx =>
          List(commonHeaders(config, flatOrArcTcsSubsystems(inst).toList, i)(ctx), gcalHeader(i), gwsHeaders(i), h)))
      }
    case DarkOrBias(inst)      => toInstrumentSys(inst) >>= { i =>
        calcInstHeader(config, inst).map(h => Reader(ctx => List(commonHeaders(config, Nil, i)(ctx), gwsHeaders(i), h)))
      }
    case st                    => TrySeq.fail(Unexpected(s"Unsupported step type $st"))
  }

}

object SeqTranslate {
  def apply(site: Site, systems: Systems[IO], settings: TranslateSettings): SeqTranslate =
    new SeqTranslate(site, systems, settings)

  private sealed trait StepType {
    val instrument: Instrument
  }

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
      case ins             => TrySeq.fail(UnrecognizedInstrument(s"inst $ins"))
    }
  }

  private final case class CelestialObject(override val instrument: Instrument) extends StepType
  private final case class Dark(override val instrument: Instrument) extends StepType
  private final case class NodAndShuffle(override val instrument: Instrument) extends StepType
  private final case class Gems(override val instrument: Instrument) extends StepType
  private final case class AltairObs(override val instrument: Instrument) extends StepType
  private final case class FlatOrArc(override val instrument: Instrument) extends StepType
  private final case class DarkOrBias(override val instrument: Instrument) extends StepType
  private case object AlignAndCalib extends StepType {
    override val instrument: Instrument = Instrument.Gpi
  }

  private def calcStepType(config: Config): TrySeq[StepType] = {
    def extractGaos(inst: Instrument): TrySeq[StepType] = config.extractAs[String](AO_SYSTEM_KEY) match {
      case Left(ConfigUtilOps.ConversionError(_, _))              => TrySeq.fail(Unexpected("Unable to get AO system from sequence"))
      case Left(ConfigUtilOps.ContentError(_))                    => TrySeq.fail(Unexpected("Logical error"))
      case Left(ConfigUtilOps.KeyNotFound(_))                     => TrySeq(CelestialObject(inst))
      case Right(AltairConstants.SYSTEM_NAME_PROP)                => TrySeq(AltairObs(inst))
      case Right(edu.gemini.spModel.gemini.gems.Gems.SYSTEM_NAME) => TrySeq(Gems(inst))
      case _                                                      => TrySeq.fail(Unexpected("Logical error reading AO system name"))
    }

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

  implicit class ResponseToResult(val r: Either[SeqexecFailure, Response]) extends AnyVal {
    def toResult: Result = r.fold(e => Result.Error(SeqexecFailure.explain(e)), r => Result.OK(r))
  }

  implicit class ResultToResult(val r: Either[SeqexecFailure, Result]) extends AnyVal {
    def toResult: Result = r.fold(e => Result.Error(SeqexecFailure.explain(e)), identity)
  }

  implicit class ConfigResultToResult[A <: Result.PartialVal](val r: Either[SeqexecFailure, ConfigResult[IO]]) extends AnyVal {
    def toResult: Result = r.fold(e => Result.Error(SeqexecFailure.explain(e)), r => Result.OK(
      Response.Configured(r.sys.resource)))
  }

  implicit class ActionResponseToAction[A <: Response](val x: SeqAction[A]) extends AnyVal {
    def toAction(kind: ActionType): Action[IO] = fromF[IO](kind, x.value.map(_.toResult))
  }

  implicit class ConfigResultToAction(val x: SeqAction[ConfigResult[IO]]) extends AnyVal {
    def toAction(kind: ActionType): Action[IO] = fromF[IO](kind, x.value.map(_.toResult))
  }

  final case class ObserveContext(t: ObserveCommand.Result => SeqAction[Result], expTime: Time) extends Result.PauseContext
}
