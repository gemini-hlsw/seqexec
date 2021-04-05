// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import java.time.Instant
import java.util.concurrent.TimeUnit

import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

import cats._
import cats.data.NonEmptyList
import cats.data.StateT
import cats.effect.Concurrent
import cats.effect.ConcurrentEffect
import cats.effect.Sync
import cats.effect.Timer
import cats.effect.concurrent.Ref
import cats.syntax.all._
import edu.gemini.seqexec.odb.SeqFailure
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.CLOUD_COVER_PROP
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.IMAGE_QUALITY_PROP
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.SKY_BACKGROUND_PROP
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.WATER_VAPOR_PROP
import edu.gemini.spModel.obsclass.ObsClass
import edu.gemini.spModel.obscomp.InstConstants.OBSERVE_TYPE_PROP
import edu.gemini.spModel.obscomp.InstConstants.OBS_CLASS_PROP
import edu.gemini.spModel.obscomp.InstConstants.SCIENCE_OBSERVE_TYPE
import edu.gemini.spModel.seqcomp.SeqConfigNames.OCS_KEY
import fs2.Pipe
import fs2.Pure
import fs2.Stream
import org.typelevel.log4cats.Logger
import lucuma.core.enum.Site
import monocle.Monocle.index
import monocle.Optional
import mouse.all._
import seqexec.engine.EventResult._
import seqexec.engine.Handle
import seqexec.engine.Result.Partial
import seqexec.engine.SystemEvent
import seqexec.engine.UserEvent
import seqexec.engine.{ Step => _, _ }
import seqexec.model.NodAndShuffleStep.PauseGracefully
import seqexec.model.NodAndShuffleStep.PendingObserveCmd
import seqexec.model.NodAndShuffleStep.StopGracefully
import seqexec.model.Notification._
import seqexec.model.Observation
import seqexec.model.StepId
import seqexec.model.UserDetails
import seqexec.model.UserPrompt.Discrepancy
import seqexec.model.UserPrompt.ObsConditionsCheckOverride
import seqexec.model.UserPrompt.SeqCheck
import seqexec.model.UserPrompt.TargetCheckOverride
import seqexec.model._
import seqexec.model.config._
import seqexec.model.enum._
import seqexec.model.events.{ SequenceStart => ClientSequenceStart, _ }
import seqexec.server.ConfigUtilOps._
import seqexec.server.EngineState.atSequence
import seqexec.server.SeqEvent._

trait SeqexecEngine[F[_]] {

  val systems: Systems[F]

  def sync(q: EventQueue[F], seqId: Observation.Id): F[Unit]

  def start(
    q:           EventQueue[F],
    id:          Observation.Id,
    user:        UserDetails,
    clientId:    ClientId,
    runOverride: RunOverride
  ): F[Unit]

  def startFrom(
    q:           EventQueue[F],
    id:          Observation.Id,
    stp:         StepId,
    clientId:    ClientId,
    runOverride: RunOverride
  ): F[Unit]

  def requestPause(q: EventQueue[F], id: Observation.Id, user: UserDetails): F[Unit]

  def requestCancelPause(q: EventQueue[F], id: Observation.Id, user: UserDetails): F[Unit]

  def setBreakpoint(
    q:      EventQueue[F],
    seqId:  Observation.Id,
    user:   UserDetails,
    stepId: StepId,
    v:      Boolean
  ): F[Unit]

  def setOperator(q: EventQueue[F], user: UserDetails, name: Operator): F[Unit]

  def setObserver(
    q:     EventQueue[F],
    seqId: Observation.Id,
    user:  UserDetails,
    name:  Observer
  ): F[Unit]

  // Systems overrides
  def setTcsEnabled(
    q:       EventQueue[F],
    seqId:   Observation.Id,
    user:    UserDetails,
    enabled: Boolean
  ): F[Unit]

  def setGcalEnabled(
    q:       EventQueue[F],
    seqId:   Observation.Id,
    user:    UserDetails,
    enabled: Boolean
  ): F[Unit]

  def setInstrumentEnabled(
    q:       EventQueue[F],
    seqId:   Observation.Id,
    user:    UserDetails,
    enabled: Boolean
  ): F[Unit]

  def setDhsEnabled(
    q:       EventQueue[F],
    seqId:   Observation.Id,
    user:    UserDetails,
    enabled: Boolean
  ): F[Unit]

  def selectSequence(
    q:        EventQueue[F],
    i:        Instrument,
    sid:      Observation.Id,
    observer: Observer,
    user:     UserDetails,
    clientId: ClientId
  ): F[Unit]

  def clearLoadedSequences(q: EventQueue[F], user: UserDetails): F[Unit]

  def resetConditions(q: EventQueue[F]): F[Unit]

  def setConditions(q: EventQueue[F], conditions: Conditions, user: UserDetails): F[Unit]

  def setImageQuality(q: EventQueue[F], iq: ImageQuality, user: UserDetails): F[Unit]

  def setWaterVapor(q: EventQueue[F], wv: WaterVapor, user: UserDetails): F[Unit]

  def setSkyBackground(q: EventQueue[F], sb: SkyBackground, user: UserDetails): F[Unit]

  def setCloudCover(q: EventQueue[F], cc: CloudCover, user: UserDetails): F[Unit]

  def setSkipMark(
    q:      EventQueue[F],
    seqId:  Observation.Id,
    user:   UserDetails,
    stepId: StepId,
    v:      Boolean
  ): F[Unit]

  def requestRefresh(q: EventQueue[F], clientId: ClientId): F[Unit]

  def stopObserve(q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit]

  def abortObserve(q: EventQueue[F], seqId: Observation.Id): F[Unit]

  def pauseObserve(q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit]

  def resumeObserve(q: EventQueue[F], seqId: Observation.Id): F[Unit]

  def addSequencesToQueue(q: EventQueue[F], qid: QueueId, seqIds: List[Observation.Id]): F[Unit]

  def addSequenceToQueue(q: EventQueue[F], qid: QueueId, seqId: Observation.Id): F[Unit]

  def removeSequenceFromQueue(q: EventQueue[F], qid: QueueId, seqId: Observation.Id): F[Unit]

  def moveSequenceInQueue(
    q:     EventQueue[F],
    qid:   QueueId,
    seqId: Observation.Id,
    delta: Int,
    cid:   ClientId
  ): F[Unit]

  def clearQueue(q: EventQueue[F], qid: QueueId): F[Unit]

  def startQueue(
    q:        EventQueue[F],
    qid:      QueueId,
    observer: Observer,
    user:     UserDetails,
    clientId: ClientId
  ): F[Unit]

  def stopQueue(q: EventQueue[F], qid: QueueId, clientId: ClientId): F[Unit]

  /**
   *  Triggers the application of a specific step configuration to a system
   */
  def configSystem(
    q:        EventQueue[F],
    sid:      Observation.Id,
    stepId:   StepId,
    sys:      Resource,
    clientID: ClientId
  ): F[Unit]

  def eventStream(q: EventQueue[F]): Stream[F, SeqexecEvent]

  // Used by tests
  def stream(p: Stream[F, EventType[F]])(
    s0:         EngineState[F]
  ): Stream[F, (EventResult[SeqEvent], EngineState[F])]
}

object SeqexecEngine {

  private class SeqexecEngineImpl[F[_]: ConcurrentEffect: Timer: Logger](
    override val systems: Systems[F],
    settings:             SeqexecEngineConfiguration,
    sm:                   SeqexecMetrics,
    translator:           SeqTranslate[F]
  )(implicit
    executeEngine:        seqexec.server.ExecEngineType[F]
  ) extends SeqexecEngine[F] {

    private val odbLoader = new ODBSequencesLoader[F](systems.odb, translator)

    override def sync(q: EventQueue[F], seqId: Observation.Id): F[Unit] =
      odbLoader.loadEvents(seqId).flatMap { e =>
        q.enqueue(
          Stream.emits(e)
        ).compile
          .drain
      }

    /**
     * Check if the resources to run a sequence are available
     * @return true if resources are available
     */
    private def checkResources(seqId: Observation.Id)(st: EngineState[F]): Boolean = {
      // Resources used by running sequences
      val used = resourcesInUse(st)

      // Resources that will be used by sequences in running queues
      val reservedByQueues = resourcesReserved(st)

      st.sequences
        .get(seqId)
        .exists(x =>
          x.seqGen.resources.intersect(used).isEmpty && (
            st.queues.values.filter(_.status(st).running).exists(_.queue.contains(seqId)) ||
              x.seqGen.resources.intersect(reservedByQueues).isEmpty
          )
        )
    }

    // Starting step is either the one given, or the first one not run
    private def findStartingStep(
      obs:    SequenceData[F],
      stepId: Option[StepId]
    ): Option[SequenceGen.StepGen[F]] = for {
      stp    <- stepId.orElse(obs.seq.currentStep.map(_.id))
      stpGen <- obs.seqGen.steps.find(_.id === stp)
    } yield stpGen

    private def findFirstCheckRequiredStep(
      obs:    SequenceData[F],
      stepId: StepId
    ): Option[SequenceGen.StepGen[F]] =
      obs.seqGen.steps.dropWhile(_.id =!= stepId).find(a => stepRequiresChecks(a.config))

    /**
     * Check if the target on the TCS matches the seqexec target
     * @return an F that returns an optional TargetMatchResult if the targets don't match
     */
    private def sequenceTcsTargetMatch(
      step: SequenceGen.StepGen[F]
    ): F[Option[TargetCheckOverride]] =
      extractTargetName(step.config)
        .map { seqTarget =>
          systems.tcsKeywordReader.sourceATarget.objectName.map { tcsTarget =>
            (seqTarget =!= tcsTarget).option(
              TargetCheckOverride(UserPrompt.Discrepancy(seqTarget, tcsTarget))
            )
          }
        }
        .getOrElse(none.pure[F])

    /**
     * Extract the target name from a step configuration. Some processing is necessary to get the same string that
     * appears in TCS.
     */
    private def extractTargetName(config: CleanConfig): Option[String] = {
      val BasePositionKey    = "Base:name"
      val SolarSystemObjects = Map(
        ("199", "Mercury"),
        ("299", "Venus"),
        ("301", "Moon"),
        ("499", "Mars"),
        ("599", "Jupiter"),
        ("699", "Saturn"),
        ("799", "Uranus"),
        ("899", "Neptune"),
        ("999", "Pluto")
      )
      val baseName           = config.extractTelescopeAs[String](BasePositionKey).toOption
      val EphemerisExtension = ".eph"

      baseName.map { x =>
        if (x.endsWith(EphemerisExtension)) {
          x.dropRight(EphemerisExtension.length)
        } else {
          SolarSystemObjects.getOrElse(x, x)
        }
      }
    }

    private def stepRequiresChecks(config: CleanConfig): Boolean = (for {
      obsClass <- config.extractObsAs[String](OBS_CLASS_PROP)
      obsType  <- config.extractObsAs[String](OBSERVE_TYPE_PROP)
    } yield (obsClass === ObsClass.SCIENCE.headerValue() ||
      obsClass === ObsClass.PROG_CAL.headerValue() ||
      obsClass === ObsClass.PARTNER_CAL.headerValue()) && obsType === SCIENCE_OBSERVE_TYPE)
      .getOrElse(false)

    private def checkCloudCover(actual: CloudCover, requested: SPSiteQuality.CloudCover): Boolean =
      actual.toInt.getOrElse(100) <= requested.getPercentage

    private def checkImageQuality(
      actual:    ImageQuality,
      requested: SPSiteQuality.ImageQuality
    ): Boolean =
      actual.toInt.getOrElse(100) <= requested.getPercentage

    private def checkSkyBackground(
      actual:    SkyBackground,
      requested: SPSiteQuality.SkyBackground
    ): Boolean =
      actual.toInt.getOrElse(100) <= requested.getPercentage

    private def checkWaterVapor(actual: WaterVapor, requested: SPSiteQuality.WaterVapor): Boolean =
      actual.toInt.getOrElse(100) <= requested.getPercentage

    val ObsConditionsProp = "obsConditions"

    private def extractCloudCover(config: CleanConfig): Option[SPSiteQuality.CloudCover] =
      config
        .extractAs[String](OCS_KEY / ObsConditionsProp / CLOUD_COVER_PROP)
        .flatMap(_.parseInt)
        .toOption
        .flatMap { x =>
          List(
            SPSiteQuality.CloudCover.PERCENT_20,
            SPSiteQuality.CloudCover.PERCENT_50,
            SPSiteQuality.CloudCover.PERCENT_70,
            SPSiteQuality.CloudCover.PERCENT_80,
            SPSiteQuality.CloudCover.PERCENT_90,
            SPSiteQuality.CloudCover.ANY
          ).find(_.getPercentage.toInt === x)
        }

    private def extractImageQuality(config: CleanConfig): Option[SPSiteQuality.ImageQuality] =
      config
        .extractAs[String](OCS_KEY / ObsConditionsProp / IMAGE_QUALITY_PROP)
        .flatMap(_.parseInt)
        .toOption
        .flatMap { x =>
          List(
            SPSiteQuality.ImageQuality.PERCENT_20,
            SPSiteQuality.ImageQuality.PERCENT_70,
            SPSiteQuality.ImageQuality.PERCENT_85,
            SPSiteQuality.ImageQuality.ANY
          ).find(_.getPercentage.toInt === x)
        }

    private def extractSkyBackground(config: CleanConfig): Option[SPSiteQuality.SkyBackground] =
      config
        .extractAs[String](OCS_KEY / ObsConditionsProp / SKY_BACKGROUND_PROP)
        .flatMap(_.parseInt)
        .toOption
        .flatMap { x =>
          List(
            SPSiteQuality.SkyBackground.PERCENT_20,
            SPSiteQuality.SkyBackground.PERCENT_50,
            SPSiteQuality.SkyBackground.PERCENT_80,
            SPSiteQuality.SkyBackground.ANY
          ).find(_.getPercentage.toInt === x)
        }

    private def extractWaterVapor(config: CleanConfig): Option[SPSiteQuality.WaterVapor] =
      config
        .extractAs[String](OCS_KEY / ObsConditionsProp / WATER_VAPOR_PROP)
        .flatMap(_.parseInt)
        .toOption
        .flatMap { x =>
          List(
            SPSiteQuality.WaterVapor.PERCENT_20,
            SPSiteQuality.WaterVapor.PERCENT_50,
            SPSiteQuality.WaterVapor.PERCENT_80,
            SPSiteQuality.WaterVapor.ANY
          ).find(_.getPercentage.toInt === x)
        }

    private def observingConditionsMatch(
      actualObsConditions: Conditions,
      step:                SequenceGen.StepGen[F]
    ): Option[ObsConditionsCheckOverride] = {

      val reqCC = extractCloudCover(step.config)
      val reqIQ = extractImageQuality(step.config)
      val reqSB = extractSkyBackground(step.config)
      val reqWV = extractWaterVapor(step.config)

      val ccCmp = reqCC.flatMap(x =>
        (!checkCloudCover(actualObsConditions.cc, x))
          .option(Discrepancy(actualObsConditions.cc.label, x.displayValue()))
      )
      val iqCmp = reqIQ.flatMap(x =>
        (!checkImageQuality(actualObsConditions.iq, x))
          .option(Discrepancy(actualObsConditions.iq.label, x.displayValue()))
      )
      val sbCmp = reqSB.flatMap(x =>
        (!checkSkyBackground(actualObsConditions.sb, x))
          .option(Discrepancy(actualObsConditions.sb.label, x.displayValue()))
      )
      val wvCmp = reqWV.flatMap(x =>
        (!checkWaterVapor(actualObsConditions.wv, x))
          .option(Discrepancy(actualObsConditions.wv.label, x.displayValue()))
      )

      (ccCmp.nonEmpty || iqCmp.nonEmpty || sbCmp.nonEmpty || wvCmp.nonEmpty)
        .option(ObsConditionsCheckOverride(ccCmp, iqCmp, sbCmp, wvCmp))

    }

    private def clearObsCmd(id: Observation.Id): HandleType[F, SeqEvent] = { (s: EngineState[F]) =>
      ((EngineState.atSequence[F](id) ^|-> SequenceData.pendingObsCmd).set(None)(s),
       SeqEvent.NullSeqEvent: SeqEvent
      )
    }.toHandle

    private def setObsCmd(id: Observation.Id, cmd: PendingObserveCmd): HandleType[F, SeqEvent] = {
      (s: EngineState[F]) =>
        ((EngineState.atSequence[F](id) ^|-> SequenceData.pendingObsCmd).set(cmd.some)(s),
         SeqEvent.NullSeqEvent: SeqEvent
        )
    }.toHandle

    // Produce a Handle that will send a SequenceStart notification to the ODB, and produces the (sequenceId, stepId)
    // if there is a valid sequence with a valid current step.
    private def sequenceStart(id: Observation.Id): HandleType[F, Option[(Observation.Id, StepId)]] =
      executeEngine.get.flatMap { s =>
        EngineState
          .atSequence(id)
          .getOption(s)
          .flatMap { seq =>
            seq.seq.currentStep.flatMap { step =>
              seq.seqGen.steps
                .find(_.id === step.id)
                .map { x =>
                  Handle
                    .fromStream[F, EngineState[F], EventType[F]](
                      Stream.eval(
                        systems.odb.sequenceStart(id, x.dataId).as(Event.nullEvent[F])
                      )
                    )
                    .as((id, step.id).some)
                }
            }
          }
          .getOrElse(executeEngine.pure(none[(Observation.Id, StepId)]))
      }

    private def startAfterCheck(
      startAction: HandleType[F, Unit],
      id:          Observation.Id
    ): HandleType[F, SeqEvent] =
      startAction.reversedStreamFlatMap(_ =>
        sequenceStart(id).map(
          _.map { case (sid, stepId) => SequenceStart(sid, stepId) }.getOrElse(NullSeqEvent)
        )
      )

    private def startChecks(
      startAction: HandleType[F, Unit],
      id:          Observation.Id,
      clientId:    ClientId,
      stepId:      Option[StepId],
      runOverride: RunOverride
    ): HandleType[F, SeqEvent] =
      executeEngine.get.flatMap { st =>
        atSequence(id)
          .getOption(st)
          .map { seq =>
            executeEngine
              .liftF {
                findStartingStep(seq, stepId)
                  .flatMap(ststp =>
                    findFirstCheckRequiredStep(seq, ststp.id).map(sp =>
                      sequenceTcsTargetMatch(sp).map { tchk =>
                        (ststp.some,
                         List(tchk, observingConditionsMatch(st.conditions, sp))
                           .collect { case Some(x) => x }
                           .widen[SeqCheck]
                        )
                      }
                    )
                  )
                  .getOrElse((none[SequenceGen.StepGen[F]], List.empty[SeqCheck]).pure[F])
              }
              .flatMap { case (stpg, checks) =>
                (checkResources(id)(st), stpg, checks, runOverride) match {
                  // Resource check fails
                  case (false, _, _, _)                             => executeEngine.unit.as[SeqEvent](Busy(id, clientId))
                  // Target check fails and no override
                  case (_, Some(stp), x :: xs, RunOverride.Default) =>
                    executeEngine.unit.as[SeqEvent](
                      RequestConfirmation(
                        UserPrompt.ChecksOverride(id, stp.id, NonEmptyList(x, xs)),
                        clientId
                      )
                    )
                  // Allowed to run
                  case _                                            => startAfterCheck(startAction, id)
                }
              }
          }
          .getOrElse(
            executeEngine.unit.as[SeqEvent](NullSeqEvent)
          ) // Trying to run a sequence that does not exists. This should never happen.
      }

    // Stars a sequence from the first non executed step. The method checks for resources conflict.
    override def start(
      q:           EventQueue[F],
      id:          Observation.Id,
      user:        UserDetails,
      clientId:    ClientId,
      runOverride: RunOverride
    ): F[Unit] =
      q.enqueue1(
        Event.modifyState[F, EngineState[F], SeqEvent](
          clearObsCmd(id) *> startChecks(executeEngine.start(id), id, clientId, none, runOverride)
        )
      )

    // Stars a sequence from an arbitrary step. All previous non executed steps are skipped.
    // The method checks for resources conflict.
    override def startFrom(
      q:           EventQueue[F],
      id:          Observation.Id,
      stp:         StepId,
      clientId:    ClientId,
      runOverride: RunOverride
    ): F[Unit] =
      q.enqueue1(
        Event.modifyState[F, EngineState[F], SeqEvent](
          clearObsCmd(id) *> startChecks(executeEngine.startFrom(id, stp),
                                         id,
                                         clientId,
                                         stp.some,
                                         runOverride
          )
        )
      )

    override def requestPause(q: EventQueue[F], id: Observation.Id, user: UserDetails): F[Unit] =
      q.enqueue1(Event.pause[F, EngineState[F], SeqEvent](id, user))

    override def requestCancelPause(
      q:    EventQueue[F],
      id:   Observation.Id,
      user: UserDetails
    ): F[Unit] =
      q.enqueue1(Event.cancelPause[F, EngineState[F], SeqEvent](id, user))

    override def setBreakpoint(
      q:      EventQueue[F],
      seqId:  Observation.Id,
      user:   UserDetails,
      stepId: StepId,
      v:      Boolean
    ): F[Unit] =
      q.enqueue1(Event.breakpoint[F, EngineState[F], SeqEvent](seqId, user, stepId, v))

    override def setOperator(q: EventQueue[F], user: UserDetails, name: Operator): F[Unit] =
      logDebugEvent(q, s"SeqexecEngine: Setting Operator name to '$name' by ${user.username}") *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            (EngineState.operator[F].set(name.some) >>> refreshSequences)
              .withEvent(SetOperator(name, user.some))
              .toHandle
          )
        )

    override def setObserver(
      q:     EventQueue[F],
      seqId: Observation.Id,
      user:  UserDetails,
      name:  Observer
    ): F[Unit] =
      logDebugEvent(
        q,
        s"SeqexecEngine: Setting Observer name to '$name' for sequence '${seqId.format}' by ${user.username}"
      ) *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.sequences[F] ^|-? index(seqId))
              .modify(SequenceData.observer.set(name.some)) >>>
              refreshSequence(seqId)).withEvent(SetObserver(seqId, user.some, name)).toHandle
          )
        )

    private def selectSequenceEvent(
      i:        Instrument,
      sid:      Observation.Id,
      observer: Observer,
      user:     UserDetails,
      clientId: ClientId
    ): EventType[F] = {
      val lens =
        (EngineState.sequences[F] ^|-? index(sid))
          .modify(SequenceData.observer.set(observer.some)) >>>
          EngineState.instrumentLoadedL[F](i).set(sid.some) >>>
          refreshSequence(sid)
      def testRunning(st: EngineState[F]): Boolean = (for {
        sels   <- st.selected.get(i)
        obsseq <- st.sequences.get(sels)
      } yield obsseq.seq.status.isRunning).getOrElse(false)

      Event.modifyState[F, EngineState[F], SeqEvent] {
        ((st: EngineState[F]) => {
          if (!testRunning(st))(lens.withEvent(AddLoadedSequence(i, sid, user, clientId)))(st)
          else (st, NotifyUser(InstrumentInUse(sid, i), clientId))
        }).toHandle
      }
    }

    override def selectSequence(
      q:        EventQueue[F],
      i:        Instrument,
      sid:      Observation.Id,
      observer: Observer,
      user:     UserDetails,
      clientId: ClientId
    ): F[Unit] =
      Sync[F]
        .delay(Instant.now)
        .flatMap { ts =>
          q.enqueue1(
            Event.logInfoMsg[F, EngineState[F], SeqEvent](
              s"User '${user.displayName}' sync and load sequence ${sid.format} on ${i.show}",
              ts
            )
          )
        } *>
        sync(q, sid) *>
        q.enqueue1(selectSequenceEvent(i, sid, observer, user, clientId))

    private def logDebugEvent(q: EventQueue[F], msg: String): F[Unit] =
      Event.logDebugMsgF[F, EngineState[F], SeqEvent](msg).flatMap(q.enqueue1)

    override def clearLoadedSequences(q: EventQueue[F], user: UserDetails): F[Unit] =
      logDebugEvent(q, "SeqexecEngine: Updating loaded sequences") *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            EngineState
              .selected[F]
              .set(Map.empty)
              .withEvent(ClearLoadedSequences(user.some))
              .toHandle
          )
        )

    override def resetConditions(q: EventQueue[F]): F[Unit] =
      logDebugEvent(q, "SeqexecEngine: Reset conditions") *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            (EngineState.conditions[F].set(Conditions.Default) >>> refreshSequences)
              .withEvent(SetConditions(Conditions.Default, None))
              .toHandle
          )
        )

    override def setConditions(
      q:          EventQueue[F],
      conditions: Conditions,
      user:       UserDetails
    ): F[Unit] =
      logDebugEvent(q, "SeqexecEngine: Setting conditions") *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            (EngineState.conditions[F].set(conditions) >>> refreshSequences)
              .withEvent(SetConditions(conditions, user.some))
              .toHandle
          )
        )

    override def setImageQuality(q: EventQueue[F], iq: ImageQuality, user: UserDetails): F[Unit] =
      logDebugEvent(q, s"SeqexecEngine: Setting image quality to $iq") *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.conditions[F] ^|-> Conditions.iq).set(iq) >>> refreshSequences)
              .withEvent(SetImageQuality(iq, user.some))
              .toHandle
          )
        )

    override def setWaterVapor(q: EventQueue[F], wv: WaterVapor, user: UserDetails): F[Unit] =
      logDebugEvent(q, s"SeqexecEngine: Setting water vapor to $wv") *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.conditions[F] ^|-> Conditions.wv).set(wv) >>> refreshSequences)
              .withEvent(SetWaterVapor(wv, user.some))
              .toHandle
          )
        )

    override def setSkyBackground(q: EventQueue[F], sb: SkyBackground, user: UserDetails): F[Unit] =
      logDebugEvent(q, s"SeqexecEngine: Setting sky background to $sb") *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.conditions[F] ^|-> Conditions.sb).set(sb) >>> refreshSequences)
              .withEvent(SetSkyBackground(sb, user.some))
              .toHandle
          )
        )

    override def setCloudCover(q: EventQueue[F], cc: CloudCover, user: UserDetails): F[Unit] =
      logDebugEvent(q, s"SeqexecEngine: Setting cloud cover to $cc") *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.conditions[F] ^|-> Conditions.cc).set(cc) >>> refreshSequences)
              .withEvent(SetCloudCover(cc, user.some))
              .toHandle
          )
        )

    override def setSkipMark(
      q:      EventQueue[F],
      seqId:  Observation.Id,
      user:   UserDetails,
      stepId: StepId,
      v:      Boolean
    ): F[Unit] =
      q.enqueue1(Event.skip[F, EngineState[F], SeqEvent](seqId, user, stepId, v))

    override def requestRefresh(q: EventQueue[F], clientId: ClientId): F[Unit] =
      q.enqueue1(Event.poll(clientId))

    private def seqQueueRefreshStream: Stream[F, Either[SeqexecFailure, EventType[F]]] = {
      val fd = Duration(settings.odbQueuePollingInterval.toSeconds, TimeUnit.SECONDS)
      Stream
        .fixedDelay[F](fd)
        .evalMap(_ => systems.odb.queuedSequences)
        .flatMap { x =>
          Stream.emit(
            Event
              .getState[F, EngineState[F], SeqEvent] { st =>
                Stream.eval(odbLoader.refreshSequenceList(x, st)).flatMap(Stream.emits).some
              }
              .asRight
          )
        }
        .handleErrorWith {
          case e: SeqFailure =>
            Stream.emit(SeqexecFailure.OdbSeqError(e).asLeft)
          case e             =>
            Stream.emit(SeqexecFailure.SeqexecException(e).asLeft)
        }
    }

    private val heartbeatPeriod: FiniteDuration = FiniteDuration(10, TimeUnit.SECONDS)

    private def heartbeatStream: Stream[F, EventType[F]] = {
      // If there is no heartbeat in 5 periods throw an error
      val noHeartbeatDetection =
        SeqexecEngine.failIfNoEmitsWithin[F, EventType[F]](5 * heartbeatPeriod,
                                                           "Engine heartbeat not detected"
        )
      Stream
        .awakeDelay[F](heartbeatPeriod)
        .as(Event.nullEvent: EventType[F])
        .through(noHeartbeatDetection.andThen(_.recoverWith { case _ =>
          Stream.eval[F, EventType[F]](Event.logErrorMsgF("Seqexec engine heartbeat undetected"))
        }))
    }

    override def eventStream(q: EventQueue[F]): Stream[F, SeqexecEvent] =
      stream(
        q.dequeue
          .mergeHaltBoth(seqQueueRefreshStream.rethrow.mergeHaltL(heartbeatStream))
      )(EngineState.default[F]).flatMap(x => Stream.eval(notifyODB(x).attempt)).flatMap {
        case Right((ev, qState)) =>
          val sequences = qState.sequences.values.map(viewSequence).toList
          toSeqexecEvent[F](ev, qState) <* Stream.eval(updateMetrics(ev, sequences))
        case Left(x)             =>
          Stream.eval(Logger[F].error(x)("Error notifying the ODB").as(NullEvent))
      }

    override def stream(p: Stream[F, EventType[F]])(
      s0:                  EngineState[F]
    ): Stream[F, (EventResult[SeqEvent], EngineState[F])] =
      executeEngine.process(iterateQueues)(p)(s0)

    override def stopObserve(q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit] =
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](setObsCmd(seqId, StopGracefully)))
        .whenA(graceful) *>
        q.enqueue1(
          Event.actionStop[F, EngineState[F], SeqEvent](seqId,
                                                        translator.stopObserve(seqId, graceful)
          )
        )

    override def abortObserve(q: EventQueue[F], seqId: Observation.Id): F[Unit] = q.enqueue1(
      Event.actionStop[F, EngineState[F], SeqEvent](seqId, translator.abortObserve(seqId))
    )

    override def pauseObserve(q: EventQueue[F], seqId: Observation.Id, graceful: Boolean): F[Unit] =
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](setObsCmd(seqId, PauseGracefully)))
        .whenA(graceful) *>
        q.enqueue1(
          Event.actionStop[F, EngineState[F], SeqEvent](seqId,
                                                        translator.pauseObserve(seqId, graceful)
          )
        )

    override def resumeObserve(q: EventQueue[F], seqId: Observation.Id): F[Unit] =
      q.enqueue1(Event.modifyState[F, EngineState[F], SeqEvent](clearObsCmd(seqId))) *>
        q.enqueue1(Event.getState[F, EngineState[F], SeqEvent](translator.resumePaused(seqId)))

    private def queueO(qid: QueueId): Optional[EngineState[F], ExecutionQueue] =
      EngineState.queues[F] ^|-? index(qid)

    private def cmdStateO(qid: QueueId): Optional[EngineState[F], BatchCommandState] =
      queueO(qid) ^|-> ExecutionQueue.cmdState

    private def addSeqs(
      qid:    QueueId,
      seqIds: List[Observation.Id]
    ): HandleType[F, List[(Observation.Id, StepId)]] =
      executeEngine.get.flatMap { st =>
        (
          for {
            q    <- st.queues.get(qid)
            seqs <- seqIds
                      .filter(sid =>
                        st.sequences
                          .get(sid)
                          .exists(os =>
                            !os.seq.status.isRunning && !os.seq.status.isCompleted && !q.queue
                              .contains(sid)
                          )
                      )
                      .some
                      .filter(_.nonEmpty)
            if seqs.nonEmpty
          } yield executeEngine.modify(queueO(qid).modify(_.addSeqs(seqs))) *>
            ((q.cmdState, q.status(st)) match {
              case (_, BatchExecState.Completed)       =>
                ((EngineState.queues[F] ^|-? index(qid) ^|-> ExecutionQueue.cmdState)
                  .set(BatchCommandState.Idle) >>> {
                  (_, List.empty[(Observation.Id, StepId)])
                }).toHandle
              case (BatchCommandState.Run(o, u, c), _) =>
                executeEngine.get.flatMap(st2 =>
                  runSequences(shouldSchedule(qid, seqs.toSet)(st2), o, u, c)
                )
              case _                                   => executeEngine.pure(List.empty[(Observation.Id, StepId)])
            })
        ).getOrElse(executeEngine.pure(List.empty[(Observation.Id, StepId)]))
      }

    override def addSequencesToQueue(
      q:      EventQueue[F],
      qid:    QueueId,
      seqIds: List[Observation.Id]
    ): F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](
        addSeqs(qid, seqIds)
          .as[SeqEvent](UpdateQueueAdd(qid, seqIds))
      )
    )

    override def addSequenceToQueue(
      q:     EventQueue[F],
      qid:   QueueId,
      seqId: Observation.Id
    ): F[Unit] =
      addSequencesToQueue(q, qid, List(seqId))

    private def removeSeq(
      qid:   QueueId,
      seqId: Observation.Id
    ): HandleType[F, List[(Observation.Id, StepId)]] =
      executeEngine.get.flatMap { st =>
        (
          for {
            q    <- st.queues.get(qid)
            if q.queue.contains(seqId)
            sstOp = st.sequences.get(seqId).map(_.seq.status)
            if q.status(st) =!= BatchExecState.Running ||
              sstOp.forall(sst => !sst.isRunning && !sst.isCompleted)
          } yield executeEngine.modify(queueO(qid).modify(_.removeSeq(seqId))) *>
            ((q.cmdState, q.status(st)) match {
              case (_, BatchExecState.Completed)       =>
                executeEngine.pure(List.empty[(Observation.Id, StepId)])
              // If removed sequence was halting the queue, then removing it frees resources to run the next sequences
              case (BatchCommandState.Run(o, u, c), _) =>
                shouldSchedule(qid, Set(seqId))(st).isEmpty.fold(
                  executeEngine.pure(List.empty[(Observation.Id, StepId)]),
                  st.sequences
                    .get(seqId)
                    .map(x => runNextsInQueue(qid, o, u, c, x.seqGen.resources))
                    .getOrElse(executeEngine.pure(List.empty[(Observation.Id, StepId)]))
                )
              case _                                   => executeEngine.pure(List.empty[(Observation.Id, StepId)])
            })
        ).getOrElse(executeEngine.pure(List.empty[(Observation.Id, StepId)]))
      }

    override def removeSequenceFromQueue(
      q:     EventQueue[F],
      qid:   QueueId,
      seqId: Observation.Id
    ): F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](
        executeEngine.get.flatMap(st =>
          removeSeq(qid, seqId)
            .map(
              UpdateQueueRemove(qid,
                                List(seqId),
                                st.queues.get(qid).map(_.queue.indexOf(seqId)).toList,
                                _
              )
            )
        )
      )
    )

    private def moveSeq(qid: QueueId, seqId: Observation.Id, delta: Int): Endo[EngineState[F]] =
      st =>
        st.queues
          .get(qid)
          .filter(_.queue.contains(seqId))
          .map { _ =>
            queueO(qid).modify(_.moveSeq(seqId, delta))(st)
          }
          .getOrElse(st)

    override def moveSequenceInQueue(
      q:     EventQueue[F],
      qid:   QueueId,
      seqId: Observation.Id,
      delta: Int,
      cid:   ClientId
    ): F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](
        executeEngine.get.flatMap(_ =>
          moveSeq(qid, seqId, delta).withEvent(UpdateQueueMoved(qid, cid, seqId, 0)).toHandle
        )
      )
    )

    private def clearQ(qid: QueueId): Endo[EngineState[F]] = st =>
      st.queues
        .get(qid)
        .filter(_.status(st) =!= BatchExecState.Running)
        .map { _ =>
          queueO(qid).modify(_.clear)(st)
        }
        .getOrElse(st)

    override def clearQueue(q: EventQueue[F], qid: QueueId): F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](
        clearQ(qid).withEvent(UpdateQueueClear(qid)).toHandle
      )
    )

    private def setObserverAndSelect(
      sid:      Observation.Id,
      observer: Observer,
      user:     UserDetails,
      clientId: ClientId
    ): HandleType[F, Unit] = Handle(
      StateT[F, EngineState[F], (Unit, Option[Stream[F, EventType[F]]])] { st: EngineState[F] =>
        (
          (EngineState.sequences[F] ^|-? index(sid))
            .getOption(st)
            .map { obsseq =>
              (EngineState
                .sequences[F]
                .modify(_ + (sid -> obsseq.copy(observer = observer.some))) >>>
                refreshSequence(sid) >>>
                EngineState.instrumentLoadedL[F](obsseq.seqGen.instrument).set(sid.some) >>> {
                  (_,
                   ((),
                    Stream[Pure, EventType[F]](
                      Event.modifyState[F, EngineState[F], SeqEvent](
                        { s: EngineState[F] => s }
                          .withEvent(
                            AddLoadedSequence(obsseq.seqGen.instrument, sid, user, clientId)
                          )
                          .toHandle
                      )
                    ).covary[F].some
                   )
                  )
                })(st)
            }
            .getOrElse((st, ((), None)))
          )
          .pure[F]
      }
    )

    private def runSequences(
      ss:       Set[Observation.Id],
      observer: Observer,
      user:     UserDetails,
      clientId: ClientId
    ): HandleType[F, List[(Observation.Id, StepId)]] =
      ss.map(sid =>
        setObserverAndSelect(sid, observer, user, clientId) *>
          executeEngine.start(sid).reversedStreamFlatMap(_ => sequenceStart(sid))
      ).toList
        .sequence
        .map(_.collect { case Some((sid, stepId)) => (sid, stepId) })

    /**
     * runQueue starts the queue. It founds the top eligible sequences in the queue, and runs them.
     */
    private def runQueue(
      qid: QueueId,
      observer: Observer,
      user:     UserDetails,
      clientId: ClientId
    ): HandleType[F, List[(Observation.Id, StepId)]] =
      executeEngine.get
        .map(findRunnableObservations(qid))
        .flatMap(runSequences(_, observer, user, clientId))

    /**
     * runNextsInQueue continues running the queue after a sequence completes. It finds the next eligible sequences in
     * the queue, and runs them.
     * At any given time a queue can be running, but one of the top eligible sequences are not. That is the case if the
     * sequence ended with an error or is stopped by the user. In both cases, the sequence should not be restarted
     * without user intervention, nor other sequence that uses the same resources should be started. Because of that,
     * runNextsInQueue only runs sequences that are now eligible because of the resources that the just completed
     * sequence has freed.
     */
    private def runNextsInQueue(
      qid:      QueueId,
      observer: Observer,
      user:     UserDetails,
      clientId: ClientId,
      freed:    Set[Resource]
    ): HandleType[F, List[(Observation.Id, StepId)]] =
      executeEngine.get
        .map(nextRunnableObservations(qid, freed))
        .flatMap(runSequences(_, observer, user, clientId))

    override def startQueue(
      q:        EventQueue[F],
      qid:      QueueId,
      observer: Observer,
      user:     UserDetails,
      clientId: ClientId
    ): F[Unit] = q.enqueue1(
      Event.modifyState[F, EngineState[F], SeqEvent](executeEngine.get.flatMap { st =>
        queueO(qid)
          .getOption(st)
          .filterNot(_.queue.isEmpty)
          .map {
            _.status(st) match {
              case BatchExecState.Idle | BatchExecState.Stopping =>
                ((EngineState.queues[F] ^|-? index(qid) ^|-> ExecutionQueue.cmdState)
                  .set(BatchCommandState.Run(observer, user, clientId)) >>> { (_, ()) }).toHandle *>
                  runQueue(qid, observer, user, clientId)
              case _                                             => executeEngine.pure(List.empty)
            }
          }
          .getOrElse(executeEngine.pure(List.empty))
          .map(StartQueue(qid, clientId, _))
      })
    )

    private def stopSequencesInQueue(qid: QueueId): HandleType[F, Unit] =
      executeEngine.get
        .map(st =>
          queueO(qid)
            .getOption(st)
            .foldMap(
              _.queue.filter(sid =>
                EngineState
                  .sequenceStateIndex[F](sid)
                  .getOption(st)
                  .exists(_.status.isRunning)
              )
            )
        )
        .flatMap(_.map(executeEngine.pause).fold(executeEngine.unit)(_ *> _))

    override def stopQueue(q: EventQueue[F], qid: QueueId, clientId: ClientId): F[Unit] =
      q.enqueue1(
        Event.modifyState[F, EngineState[F], SeqEvent](
          executeEngine.get
            .flatMap { st =>
              queueO(qid)
                .getOption(st)
                .map {
                  _.status(st) match {
                    case BatchExecState.Running =>
                      (cmdStateO(qid).set(BatchCommandState.Stop) >>> { (_, ()) }).toHandle *>
                        stopSequencesInQueue(qid)
                    case BatchExecState.Waiting =>
                      (cmdStateO(qid).set(BatchCommandState.Stop) >>> { (_, ()) }).toHandle
                    case _                      => executeEngine.unit
                  }
                }
                .getOrElse(executeEngine.unit)
            }
            .as(StopQueue(qid, clientId))
        )
      )

    private def iterateQueues: PartialFunction[SystemEvent[F], HandleType[F, Unit]] = {
      // Responds to events that could trigger the scheduling of the next sequence in the queue:
      case SystemEvent.Finished(sid) =>
        executeEngine.get.flatMap(st =>
          st.sequences
            .get(sid)
            .flatMap { seq =>
              val freed = seq.seqGen.resources
              st.queues.collectFirst {
                case (qid, q @ ExecutionQueue(_, BatchCommandState.Run(observer, user, clid), _))
                    if q.status(st) =!= BatchExecState.Completed =>
                  runNextsInQueue(qid, observer, user, clid, freed).flatMap { l =>
                    Handle.fromStream(
                      Stream.emit[F, EventType[F]](
                        Event.modifyState[F, EngineState[F], SeqEvent](
                          executeEngine.pure(SequencesStart(l))
                        )
                      )
                    )
                  }
              }
            }
            .getOrElse(executeEngine.unit)
        )
    }

    private def configSystemCheck(sid: Observation.Id, sys: Resource)(
      st:                              EngineState[F]
    ): Boolean = {
      // Resources used by running sequences
      val used = resourcesInUse(st)

      // Resources reserved by running queues, excluding `sid` to prevent self blocking
      val reservedByQueues = resourcesReserved(EngineState.sequences[F].modify(_ - sid)(st))

      !(used ++ reservedByQueues).contains(sys)
    }

    private def configSystemHandle(
      sid:      Observation.Id,
      stepId:   StepId,
      sys:      Resource,
      clientID: ClientId
    ): HandleType[F, SeqEvent] =
      executeEngine.get.flatMap { st =>
        if (configSystemCheck(sid, sys)(st)) {
          st.sequences
            .get(sid)
            .flatMap(_.seqGen.configActionCoord(stepId, sys))
            .map(c =>
              executeEngine.startSingle(ActionCoords(sid, c)).map[SeqEvent] {
                case EventResult.Outcome.Ok => StartSysConfig(sid, stepId, sys)
                case _                      => NullSeqEvent
              }
            )
            .getOrElse(executeEngine.pure(NullSeqEvent))
        } else {
          executeEngine.pure(ResourceBusy(sid, stepId, sys, clientID))
        }
      }

    /**
     *  Triggers the application of a specific step configuration to a system
     */
    override def configSystem(
      q:        EventQueue[F],
      sid:      Observation.Id,
      stepId:   StepId,
      sys:      Resource,
      clientID: ClientId
    ): F[Unit] =
      q.enqueue1(
        Event.modifyState[F, EngineState[F], SeqEvent](
          configSystemHandle(sid, stepId, sys, clientID)
        )
      )

    def notifyODB(
      i: (EventResult[SeqEvent], EngineState[F])
    ): F[(EventResult[SeqEvent], EngineState[F])] =
      (i match {
        case (SystemUpdate(SystemEvent.Failed(id, _, e), _), _) =>
          Logger[F].error(s"Error executing ${id.format} due to $e") *>
            systems.odb
              .obsAbort(id, e.msg)
              .ensure(
                SeqexecFailure
                  .Unexpected("Unable to send ObservationAborted message to ODB.")
              )(identity)
        case (SystemUpdate(SystemEvent.Executed(id), _), st)
            if EngineState
              .sequenceStateIndex(id)
              .getOption(st)
              .exists(_.status === SequenceState.Idle) =>
          systems.odb.obsPause(id, "Sequence paused by user").void
        case (SystemUpdate(SystemEvent.Finished(id), _), _)     => systems.odb.sequenceEnd(id).void
        case _                                                  => Applicative[F].unit
      }).as(i)

    /**
     * Update some metrics based on the event types
     */
    def updateMetrics(e: EventResult[SeqEvent], sequences: List[SequenceView]): F[Unit] = {
      def instrument(id: Observation.Id): Option[Instrument] =
        sequences.find(_.id === id).map(_.metadata.instrument)

      (e match {
        // TODO Add metrics for more events
        case UserCommandResponse(ue, _, _) =>
          ue match {
            case UserEvent.Start(id, _, _) =>
              instrument(id).map(sm.startRunning[F]).getOrElse(Sync[F].unit)
            case _                         => Sync[F].unit
          }
        case SystemUpdate(se, _)           =>
          se match {
            case _ => Sync[F].unit
          }
        case _                             => Sync[F].unit
      }).flatMap(_ => Sync[F].unit)
    }

    private def updateSequenceEndo(
      seqId:  Observation.Id,
      obsseq: SequenceData[F]
    ): Endo[EngineState[F]] = st =>
      executeEngine.update(seqId,
                           toStepList(obsseq.seqGen,
                                      obsseq.overrides,
                                      HeaderExtraData(st.conditions, st.operator, obsseq.observer)
                           )
      )(st)

    private def refreshSequence(id: Observation.Id): Endo[EngineState[F]] = (st: EngineState[F]) =>
      {
        st.sequences.get(id).map(obsseq => updateSequenceEndo(id, obsseq)).foldLeft(st) {
          case (s, f) => f(s)
        }
      }

    private def refreshSequences: Endo[EngineState[F]] = (st: EngineState[F]) => {
      st.sequences.map { case (id, obsseq) => updateSequenceEndo(id, obsseq) }.foldLeft(st) {
        case (s, f) => f(s)
      }
    }

    override def setTcsEnabled(
      q:       EventQueue[F],
      seqId:   Observation.Id,
      user:    UserDetails,
      enabled: Boolean
    ): F[Unit] =
      logDebugEvent(
        q,
        s"SeqexecEngine: Setting TCS enabled flag to '$enabled' for sequence '${seqId.format}' by ${user.username}"
      ) *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.sequences[F] ^|-? index(seqId)).modify(SequenceData.overrides.modify {
              x => if (enabled) x.enableTcs else x.disableTcs
            }) >>>
              refreshSequence(seqId)).withEvent(SetTcsEnabled(seqId, user.some, enabled)).toHandle
          )
        )

    override def setGcalEnabled(
      q:       EventQueue[F],
      seqId:   Observation.Id,
      user:    UserDetails,
      enabled: Boolean
    ): F[Unit] =
      logDebugEvent(
        q,
        s"SeqexecEngine: Setting Gcal enabled flag to '$enabled' for sequence '${seqId.format}' by ${user.username}"
      ) *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.sequences[F] ^|-? index(seqId)).modify(SequenceData.overrides.modify {
              x => if (enabled) x.enableGcal else x.disableGcal
            }) >>>
              refreshSequence(seqId)).withEvent(SetGcalEnabled(seqId, user.some, enabled)).toHandle
          )
        )

    override def setInstrumentEnabled(
      q:       EventQueue[F],
      seqId:   Observation.Id,
      user:    UserDetails,
      enabled: Boolean
    ): F[Unit] =
      logDebugEvent(
        q,
        s"SeqexecEngine: Setting instrument enabled flag to '$enabled' for sequence '${seqId.format}' by ${user.username}"
      ) *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.sequences[F] ^|-? index(seqId)).modify(SequenceData.overrides.modify {
              x => if (enabled) x.enableInstrument else x.disableInstrument
            }) >>>
              refreshSequence(seqId))
              .withEvent(SetInstrumentEnabled(seqId, user.some, enabled))
              .toHandle
          )
        )

    override def setDhsEnabled(
      q:       EventQueue[F],
      seqId:   Observation.Id,
      user:    UserDetails,
      enabled: Boolean
    ): F[Unit] =
      logDebugEvent(
        q,
        s"SeqexecEngine: Setting DHS enabled flag to '$enabled' for sequence '${seqId.format}' by ${user.username}"
      ) *>
        q.enqueue1(
          Event.modifyState[F, EngineState[F], SeqEvent](
            ((EngineState.sequences[F] ^|-? index(seqId)).modify(SequenceData.overrides.modify {
              x => if (enabled) x.enableDhs else x.disableDhs
            }) >>>
              refreshSequence(seqId)).withEvent(SetDhsEnabled(seqId, user.some, enabled)).toHandle
          )
        )
  }

  def createTranslator[F[_]: Sync: Logger](site: Site, systems: Systems[F]): F[SeqTranslate[F]] =
    SeqTranslate(site, systems)

  private def splitWhere[A](l: List[A])(p: A => Boolean): (List[A], List[A]) =
    l.splitAt(l.indexWhere(p))

  private def systemsBeingConfigured[F[_]](st: EngineState[F]): Set[Resource] =
    st.sequences.values
      .filter(d => d.seq.status.isError || d.seq.status.isIdle)
      .toList
      .flatMap(s =>
        s.seq.getSingleActionStates
          .filter(_._2.started)
          .keys
          .toList
          .mapFilter(s.seqGen.resourceAtCoords)
      )
      .toSet

  /**
   * Resource in use = Resources used by running sequences, plus the systems that are being configured because a user
   * commanded a manual configuration apply.
   */
  private def resourcesInUse[F[_]](st: EngineState[F]): Set[Resource] =
    st.sequences.values.toList
      .mapFilter(s => s.seq.status.isRunning.option(s.seqGen.resources))
      .foldK ++
      systemsBeingConfigured(st)

  /**
   * Resources reserved by running queues.
   */
  private def resourcesReserved[F[_]](st: EngineState[F]): Set[Resource] = {
    def reserved(q: ExecutionQueue): Set[Resource] = q.queue
      .fproduct(st.sequences.get)
      .collect {
        case (_, Some(s)) if s.seq.status.isIdle => s.seqGen.resources
      }
      .foldK

    val runningQs = st.queues.values.filter(_.status(st).running)

    runningQs.map(reserved).toList.foldK

  }

  /**
   * Creates a stream that will follow a heartbeat and raise an error if the heartbeat
   * doesn't get emitted for timeout
   *
   * Credit: Fabio Labella
   * https://gitter.im/functional-streams-for-scala/fs2?at=5e0a6efbfd580457e79aaf0a
   */
  def failIfNoEmitsWithin[F[_]: Concurrent: Timer, A](
    timeout: FiniteDuration,
    msg:     String
  ): Pipe[F, A, A] = in => {
    import scala.concurrent.TimeoutException
    def now = Timer[F].clock.monotonic(NANOSECONDS).map(_.nanos)

    Stream.eval(now.flatMap(Ref[F].of)).flatMap { lastActivityAt =>
      in.evalTap(_ => now.flatMap(lastActivityAt.set))
        .concurrently {
          Stream.repeatEval {
            (now, lastActivityAt.get)
              .mapN(_ - _)
              .flatMap { elapsed =>
                val t = timeout - elapsed

                Sync[F]
                  .raiseError[Unit](new TimeoutException(msg))
                  .whenA(t <= 0.nanos) >> Timer[F].sleep(t)
              }
          }
        }
    }
  }

  /**
   * Find the observations in an execution queue that would be run next, taking into account the resources required by
   * each observation and the resources currently in use.
   * The order in the queue defines the priority of the observations.
   * Failed or stopped sequences in the queue keep their instruments taken, preventing that the queue starts other
   * sequences for those instruments.
   * @param qid The execution queue id
   * @param st The current engine state
   * @return The set of all observations in the execution queue `qid` that can be started to run
   *         in parallel.
   */
  def findRunnableObservations[F[_]](qid: QueueId)(st: EngineState[F]): Set[Observation.Id] = {
    // Set of all resources in use
    val used = resourcesInUse(st)
    // For each observation in the queue that is not yet run, retrieve the required resources
    val obs  = st.queues
      .get(qid)
      .map(_.queue.fproduct(st.sequences.get).collect {
        case (id, Some(s)) if !s.seq.status.isRunning && !s.seq.status.isCompleted =>
          id -> s.seqGen.resources
      })
      .orEmpty

    obs
      .foldLeft((used, Set.empty[Observation.Id])) { case ((u, a), (oid, res)) =>
        if (u.intersect(res).isEmpty)
          (u ++ res, a + oid)
        else (u, a)
      }
      ._2
  }

  /**
   * Find next runnable observations given that a set of resources has just being released
   * @param qid The execution queue id
   * @param st The current engine state
   * @param freed Resources that were freed
   * @return The set of all observations in the execution queue `qid` that can be started to run
   *         in parallel.
   */
  private def nextRunnableObservations[F[_]](qid: QueueId, freed: Set[Resource])(
    st:                                           EngineState[F]
  ): Set[Observation.Id] = {
    // Set of all resources in use
    val used = resourcesInUse(st)
    // For each observation in the queue that is not yet run, retrieve the required resources
    val obs  = st.queues
      .get(qid)
      .map(_.queue.fproduct(st.sequences.get).collect {
        case (id, Some(s)) if !s.seq.status.isRunning && !s.seq.status.isCompleted =>
          id -> s.seqGen.resources
      })
      .orEmpty

    // Calculate instruments reserved by failed sequences in the queue
    val resFailed: Set[Resource] = st.queues
      .get(qid)
      .map(
        _.queue.mapFilter(
          st.sequences
            .get(_)
            .flatMap(s => s.seq.status.isError.option(s.seqGen.instrument))
        )
      )
      .orEmpty
      .toSet

    obs
      .foldLeft((used ++ resFailed, Set[Observation.Id]())) { case ((u, a), (oid, res)) =>
        if (u.intersect(res).isEmpty && freed.intersect(res).nonEmpty) (u ++ res, a + oid)
        else (u, a)
      }
      ._2
  }

  /**
   * shouldSchedule checks if a set of sequences are candidates for been run in a queue.
   * It is used to check if sequences added to a queue should be started.
   */
  private def shouldSchedule[F[_]](qid: QueueId, sids: Set[Observation.Id])(
    st:                                 EngineState[F]
  ): Set[Observation.Id] =
    findRunnableObservations(qid)(st).intersect(sids)

  /**
   * Build the seqexec and setup epics
   */
  def build[F[_]: ConcurrentEffect: Timer: Logger](
    site:          Site,
    systems:       Systems[F],
    conf:          SeqexecEngineConfiguration,
    metrics:       SeqexecMetrics
  )(implicit
    executeEngine: ExecEngineType[F]
  ): F[SeqexecEngine[F]] =
    createTranslator(site, systems)
      .map(new SeqexecEngineImpl[F](systems, conf, metrics, _))

  private def modifyStateEvent[F[_]](
    v:   SeqEvent,
    svs: => SequencesQueue[SequenceView]
  ): Stream[F, SeqexecEvent] =
    v match {
      case NullSeqEvent                       => Stream.empty
      case SetOperator(_, _)                  => Stream.emit(OperatorUpdated(svs))
      case SetObserver(_, _, _)               => Stream.emit(ObserverUpdated(svs))
      case SetTcsEnabled(_, _, _)             => Stream.emit(OverridesUpdated(svs))
      case SetGcalEnabled(_, _, _)            => Stream.emit(OverridesUpdated(svs))
      case SetInstrumentEnabled(_, _, _)      => Stream.emit(OverridesUpdated(svs))
      case SetDhsEnabled(_, _, _)             => Stream.emit(OverridesUpdated(svs))
      case AddLoadedSequence(i, s, _, c)      => Stream.emit(LoadSequenceUpdated(i, s, svs, c))
      case ClearLoadedSequences(_)            => Stream.emit(ClearLoadedSequencesUpdated(svs))
      case SetConditions(_, _)                => Stream.emit(ConditionsUpdated(svs))
      case SetImageQuality(_, _)              => Stream.emit(ConditionsUpdated(svs))
      case SetWaterVapor(_, _)                => Stream.emit(ConditionsUpdated(svs))
      case SetSkyBackground(_, _)             => Stream.emit(ConditionsUpdated(svs))
      case SetCloudCover(_, _)                => Stream.emit(ConditionsUpdated(svs))
      case LoadSequence(id)                   => Stream.emit(SequenceLoaded(id, svs))
      case UnloadSequence(id)                 => Stream.emit(SequenceUnloaded(id, svs))
      case NotifyUser(m, cid)                 => Stream.emit(UserNotification(m, cid))
      case RequestConfirmation(m, cid)        => Stream.emit(UserPromptNotification(m, cid))
      case UpdateQueueAdd(qid, seqs)          =>
        Stream.emit(QueueUpdated(QueueManipulationOp.AddedSeqs(qid, seqs), svs))
      case UpdateQueueRemove(qid, s, p, l)    =>
        Stream.emits(
          QueueUpdated(QueueManipulationOp.RemovedSeqs(qid, s, p), svs)
            +: l.map { case (sid, step) => ClientSequenceStart(sid, step, svs) }
        )
      case UpdateQueueMoved(qid, cid, oid, p) =>
        Stream.emit(QueueUpdated(QueueManipulationOp.Moved(qid, cid, oid, p), svs))
      case UpdateQueueClear(qid)              => Stream.emit(QueueUpdated(QueueManipulationOp.Clear(qid), svs))
      case StartQueue(qid, _, l)              =>
        Stream.emits(
          QueueUpdated(QueueManipulationOp.Started(qid), svs)
            +: l.map { case (sid, step) => ClientSequenceStart(sid, step, svs) }
        )
      case StopQueue(qid, _)                  => Stream.emit(QueueUpdated(QueueManipulationOp.Stopped(qid), svs))
      case StartSysConfig(sid, stepId, res)   =>
        Stream.emit(SingleActionEvent(SingleActionOp.Started(sid, stepId, res)))
      case SequenceStart(sid, stepId)         => Stream.emit(ClientSequenceStart(sid, stepId, svs))
      case SequencesStart(l)                  =>
        Stream.emits(l.map { case (sid, step) => ClientSequenceStart(sid, step, svs) })
      case Busy(id, cid)                      => Stream.emit(UserNotification(ResourceConflict(id), cid))
      case ResourceBusy(id, sid, res, cid)    =>
        Stream.emit(UserNotification(SubsystemBusy(id, sid, res), cid))
    }

  private def executionQueueViews[F[_]](
    st: EngineState[F]
  ): SortedMap[QueueId, ExecutionQueueView] =
    SortedMap(st.queues.map { case (qid, q) =>
      qid -> ExecutionQueueView(qid, q.name, q.cmdState, q.status(st), q.queue)
    }.toList: _*)

  private def viewSequence[F[_]](obsSeq: SequenceData[F]): SequenceView = {
    val st         = obsSeq.seq
    val seq        = st.toSequence
    val instrument = obsSeq.seqGen.instrument

    def resources(s:     SequenceGen.StepGen[F]): List[Resource] = s match {
      case s: SequenceGen.PendingStepGen[F] => s.resources.toList
      case _                                => List.empty
    }
    def engineSteps(seq: Sequence[F]): List[Step]                =
      obsSeq.seqGen.steps.zip(seq.steps).map { case (a, b) =>
        StepsView
          .stepsView(instrument)
          .stepView(a,
                    b,
                    resources(a).mapFilter(x =>
                      obsSeq.seqGen
                        .configActionCoord(a.id, x)
                        .map(i => (x, obsSeq.seq.getSingleState(i).actionStatus))
                    ),
                    obsSeq.pendingObsCmd
          )
      } match {
        // The sequence could be empty
        case Nil => Nil
        // Find first Pending Step when no Step is Running and mark it as Running
        case steps
            if Sequence.State.isRunning(st) && steps.forall(_.status =!= StepState.Running) =>
          val (xs, y :: ys) = splitWhere(steps)(_.status === StepState.Pending)
          xs ++ (Step.status.set(StepState.Running)(y) :: ys)
        case steps
            if st.status === SequenceState.Idle && steps.exists(_.status === StepState.Running) =>
          val (xs, y :: ys) = splitWhere(steps)(_.status === StepState.Running)
          xs ++ (Step.status.set(StepState.Paused)(y) :: ys)
        case x   => x
      }

    // TODO: Implement willStopIn
    SequenceView(seq.id,
                 SequenceMetadata(instrument, obsSeq.observer, obsSeq.seqGen.title),
                 st.status,
                 obsSeq.overrides,
                 engineSteps(seq),
                 None
    )
  }

  private def toSeqexecEvent[F[_]](
    ev:     EventResult[SeqEvent],
    qState: EngineState[F]
  ): Stream[F, SeqexecEvent] = {
    val sequences = qState.sequences.values.map(viewSequence).toList
    // Building the view is a relatively expensive operation
    // By putting it into a def we only incur that cost if the message requires it
    def svs       =
      SequencesQueue(EngineState.selected[F].get(qState),
                     EngineState.conditions[F].get(qState),
                     EngineState.operator[F].get(qState),
                     executionQueueViews(qState),
                     sequences
      )

    ev match {
      case UserCommandResponse(ue, _, uev) =>
        ue match {
          case UserEvent.Start(id, _, _)        =>
            val rs = sequences.find(_.id === id).flatMap(_.runningStep)
            Stream.emit(ClientSequenceStart(id, rs.foldMap(_.last), svs))
          case UserEvent.Pause(_, _)            => Stream.emit(SequencePauseRequested(svs))
          case UserEvent.CancelPause(id, _)     => Stream.emit(SequencePauseCanceled(id, svs))
          case UserEvent.Breakpoint(_, _, _, _) => Stream.emit(StepBreakpointChanged(svs))
          case UserEvent.SkipMark(_, _, _, _)   => Stream.emit(StepSkipMarkChanged(svs))
          case UserEvent.Poll(cid)              => Stream.emit(SequenceRefreshed(svs, cid))
          case UserEvent.GetState(_)            => Stream.empty
          case UserEvent.ModifyState(_)         => modifyStateEvent(uev.getOrElse(NullSeqEvent), svs)
          case UserEvent.ActionStop(_, _)       => Stream.emit(ActionStopRequested(svs))
          case UserEvent.LogDebug(_, _)         => Stream.empty
          case UserEvent.LogInfo(m, ts)         => Stream.emit(ServerLogMessage(ServerLogLevel.INFO, ts, m))
          case UserEvent.LogWarning(m, ts)      =>
            Stream.emit(ServerLogMessage(ServerLogLevel.WARN, ts, m))
          case UserEvent.LogError(m, ts)        =>
            Stream.emit(ServerLogMessage(ServerLogLevel.ERROR, ts, m))
          case UserEvent.ActionResume(_, _, _)  => Stream.emit(SequenceUpdated(svs))
        }
      case SystemUpdate(se, _)             =>
        se match {
          // TODO: Sequence completed event not emitted by engine.
          case SystemEvent.Completed(_, _, _, _)                                    => Stream.emit(SequenceUpdated(svs))
          case SystemEvent.StopCompleted(id, _, _, _)                               => Stream.emit(SequenceStopped(id, svs))
          case SystemEvent.Aborted(id, _, _, _)                                     => Stream.emit(SequenceAborted(id, svs))
          case SystemEvent.PartialResult(_, _, _, Partial(_: InternalPartialVal))   => Stream.empty
          case SystemEvent.PartialResult(i, s, _, Partial(ObsProgress(t, r, v)))    =>
            Stream.emit(ObservationProgressEvent(ObservationProgress(i, s, t, r.self, v)))
          case SystemEvent.PartialResult(i, s, _, Partial(NSProgress(t, r, v, u)))  =>
            Stream.emit(ObservationProgressEvent(NSObservationProgress(i, s, t, r.self, v, u)))
          case SystemEvent.PartialResult(_, _, _, Partial(FileIdAllocated(fileId))) =>
            Stream.emit(FileIdStepExecuted(fileId, svs))
          case SystemEvent.PartialResult(_, _, _, _)                                =>
            Stream.emit(SequenceUpdated(svs))
          case SystemEvent.Failed(id, _, _)                                         => Stream.emit(SequenceError(id, svs))
          case SystemEvent.Busy(id, clientId)                                       =>
            Stream.emit(UserNotification(ResourceConflict(id), clientId))
          case SystemEvent.Executed(s)                                              => Stream.emit(StepExecuted(s, svs))
          case SystemEvent.Executing(_)                                             => Stream.emit(SequenceUpdated(svs))
          case SystemEvent.Finished(_)                                              => Stream.emit(SequenceCompleted(svs))
          case SystemEvent.Null                                                     => Stream.empty
          case SystemEvent.Paused(id, _, _)                                         => Stream.emit(ExposurePaused(id, svs))
          case SystemEvent.BreakpointReached(id)                                    => Stream.emit(SequencePaused(id, svs))
          case SystemEvent.SingleRunCompleted(c, _)                                 =>
            Stream.emit(
              singleActionEvent[F, SingleActionOp.Completed](c, qState, SingleActionOp.Completed)
            )
          case SystemEvent.SingleRunFailed(c, r)                                    =>
            Stream.emit(
              singleActionEvent[F, SingleActionOp.Error](c,
                                                         qState,
                                                         SingleActionOp.Error.apply(_, _, _, r.msg)
              )
            )
        }
    }
  }

  private def singleActionEvent[F[_], S <: SingleActionOp](
    c:      ActionCoords,
    qState: EngineState[F],
    f:      (Observation.Id, StepId, Resource) => S
  ): SeqexecEvent =
    qState.sequences
      .get(c.sid)
      .flatMap(_.seqGen.resourceAtCoords(c.actCoords))
      .map(res => SingleActionEvent(f(c.sid, c.actCoords.stepId, res)))
      .getOrElse(NullEvent)

}
