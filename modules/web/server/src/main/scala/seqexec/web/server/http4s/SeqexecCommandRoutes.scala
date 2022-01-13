// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.Sync
import cats.syntax.all._
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.middleware.GZip
import seqexec.model.Observation
import seqexec.model._
import seqexec.model.enum._
import seqexec.server
import seqexec.server.SeqexecEngine
import seqexec.web.server.http4s.OptionalRunOverride
import seqexec.web.server.http4s.encoder._
import seqexec.web.server.security.AuthenticationService
import seqexec.web.server.security.Http4sAuthentication
import seqexec.web.server.security.TokenRefresher

/**
 * Rest Endpoints under the /api route
 */
class SeqexecCommandRoutes[F[_]: Sync](
  auth:       AuthenticationService[F],
  inputQueue: server.EventQueue[F],
  se:         SeqexecEngine[F]
) extends BooEncoders
    with Http4sDsl[F] {

  // Handles authentication
  private val httpAuthentication = new Http4sAuthentication(auth)

  private val commandServices: AuthedRoutes[UserDetails, F] = AuthedRoutes.of {
    case POST -> Root / ObsIdVar(obsId) / "start" / ObserverVar(obs) / ClientIDVar(
          clientId
        ) :? OptionalRunOverride(
          runOverride
        ) as user =>
      se.start(inputQueue,
               obsId,
               user,
               obs,
               clientId,
               runOverride.getOrElse(RunOverride.Default)
      ) *>
        Ok(s"Started sequence ${obsId.format}")

    case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "startFrom" / ObserverVar(
          obs
        ) / ClientIDVar(
          clientId
        ) :? OptionalRunOverride(runOverride) as _ =>
      se.startFrom(inputQueue,
                   obsId,
                   obs,
                   stepId,
                   clientId,
                   runOverride.getOrElse(RunOverride.Default)
      ) *>
        Ok(s"Started sequence ${obsId.format} from step $stepId")

    case POST -> Root / ObsIdVar(obsId) / "pause" / ObserverVar(obs) as user =>
      se.requestPause(inputQueue, obsId, obs, user) *>
        Ok(s"Pause sequence ${obsId.format}")

    case POST -> Root / ObsIdVar(obsId) / "cancelpause" / ObserverVar(obs) as user =>
      se.requestCancelPause(inputQueue, obsId, obs, user) *>
        Ok(s"Cancel Pause sequence ${obsId.format}")

    case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "breakpoint" / ObserverVar(
          obs
        ) / BooleanVar(
          bp
        ) as user =>
      se.setBreakpoint(inputQueue, obsId, user, obs, stepId, bp) *>
        Ok(s"Set breakpoint in step $stepId of sequence ${obsId.format}")

    case POST -> Root / ObsIdVar(obsId) / "sync" as _ =>
      for {
        u    <- se.sync(inputQueue, obsId).attempt
        resp <- u.fold(_ => NotFound(s"Not found sequence $obsId"),
                       _ => Ok(s"Sync requested for ${obsId.format}")
                )
      } yield resp

    case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "skip" / ObserverVar(
          obs
        ) / BooleanVar(bp) as user =>
      se.setSkipMark(inputQueue, obsId, user, obs, stepId, bp) *>
        Ok(s"Set skip mark in step $stepId of sequence ${obsId.format}")

    case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "stop" / ObserverVar(
          obs
        ) as user =>
      se.stopObserve(inputQueue, obsId, obs, user, graceful = false) *>
        Ok(s"Stop requested for ${obsId.format} on step $stepId")

    case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "stopGracefully" / ObserverVar(
          obs
        ) as user =>
      se.stopObserve(inputQueue, obsId, obs, user, graceful = true) *>
        Ok(s"Stop gracefully requested for ${obsId.format} on step $stepId")

    case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "abort" / ObserverVar(obs) as user =>
      se.abortObserve(inputQueue, obsId, obs, user) *>
        Ok(s"Abort requested for ${obsId.format} on step $stepId")

    case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "pauseObs" / ObserverVar(
          obs
        ) as user =>
      se.pauseObserve(inputQueue, obsId, obs, user, graceful = false) *>
        Ok(s"Pause observation requested for ${obsId.format} on step $stepId")

    case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "pauseObsGracefully" / ObserverVar(
          obs
        ) as user =>
      se.pauseObserve(inputQueue, obsId, obs, user, graceful = true) *>
        Ok(s"Pause observation gracefully requested for ${obsId.format} on step $stepId")

    case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "resumeObs" / ObserverVar(
          obs
        ) as user =>
      se.resumeObserve(inputQueue, obsId, obs, user) *>
        Ok(s"Resume observation requested for ${obsId.format} on step $stepId")

    case POST -> Root / "operator" / OperatorVar(op) as user =>
      se.setOperator(inputQueue, user, op) *> Ok(s"Set operator name to '${op.value}'")

    case POST -> Root / ObsIdVar(obsId) / "observer" / ObserverVar(obs) as user =>
      se.setObserver(inputQueue, obsId, user, obs) *>
        Ok(s"Set observer name to '${obs.value}' for sequence ${obsId.format}")

    case POST -> Root / ObsIdVar(obsId) / "tcsEnabled" / BooleanVar(tcsEnabled) as user =>
      se.setTcsEnabled(inputQueue, obsId, user, tcsEnabled) *>
        Ok(s"Set TCS enable flag to '$tcsEnabled' for sequence ${obsId.format}")

    case POST -> Root / ObsIdVar(obsId) / "gcalEnabled" / BooleanVar(gcalEnabled) as user =>
      se.setGcalEnabled(inputQueue, obsId, user, gcalEnabled) *>
        Ok(s"Set GCAL enable flag to '$gcalEnabled' for sequence ${obsId.format}")

    case POST -> Root / ObsIdVar(obsId) / "instEnabled" / BooleanVar(instEnabled) as user =>
      se.setInstrumentEnabled(inputQueue, obsId, user, instEnabled) *>
        Ok(s"Set instrument enable flag to '$instEnabled' for sequence ${obsId.format}")

    case POST -> Root / ObsIdVar(obsId) / "dhsEnabled" / BooleanVar(dhsEnabled) as user =>
      se.setDhsEnabled(inputQueue, obsId, user, dhsEnabled) *>
        Ok(s"Set DHS enable flag to '$dhsEnabled' for sequence ${obsId.format}")

    case req @ POST -> Root / "iq" as user =>
      req.req.decode[ImageQuality](iq =>
        se.setImageQuality(inputQueue, iq, user) *>
          Ok(s"Set image quality to $iq")
      )

    case req @ POST -> Root / "wv" as user =>
      req.req.decode[WaterVapor](wv =>
        se.setWaterVapor(inputQueue, wv, user) *> Ok(s"Set water vapor to $wv")
      )

    case req @ POST -> Root / "sb" as user =>
      req.req.decode[SkyBackground](sb =>
        se.setSkyBackground(inputQueue, sb, user) *>
          Ok(s"Set sky background to $sb")
      )

    case req @ POST -> Root / "cc" as user =>
      req.req.decode[CloudCover](cc =>
        se.setCloudCover(inputQueue, cc, user) *> Ok(s"Set cloud cover to $cc")
      )

    case POST -> Root / "load" / InstrumentVar(i) / ObsIdVar(obsId) / ObserverVar(
          observer
        ) / ClientIDVar(clientId) as user =>
      se.selectSequence(inputQueue, i, obsId, observer, user, clientId) *>
        Ok(s"Set selected sequence $obsId for $i")

    case POST -> Root / "unload" / "all" as user =>
      se.clearLoadedSequences(inputQueue, user) *> Ok(s"Queue cleared")

    case req @ POST -> Root / "queue" / QueueIdVar(qid) / "add" as _ =>
      req.req.decode[List[Observation.Id]](ids =>
        se.addSequencesToQueue(inputQueue, qid, ids) *>
          Ok(s"${ids.map(_.format).mkString(",")} added to queue $qid")
      )

    case POST -> Root / "queue" / QueueIdVar(qid) / "add" / ObsIdVar(obsId) as _ =>
      se.addSequenceToQueue(inputQueue, qid, obsId) *>
        Ok(s"${obsId.format} added to queue $qid")

    case POST -> Root / "queue" / QueueIdVar(qid) / "remove" / ObsIdVar(obsId) as _ =>
      se.removeSequenceFromQueue(inputQueue, qid, obsId) *>
        Ok(s"${obsId.format} removed from queue $qid")

    case POST -> Root / "queue" / QueueIdVar(qid) / "move" / ObsIdVar(obsId) / IntVar(
          delta
        ) / ClientIDVar(clientId) as _ =>
      se.moveSequenceInQueue(inputQueue, qid, obsId, delta, clientId) *>
        Ok(s"${obsId.format} moved $delta positions in queue $qid")

    case POST -> Root / "queue" / QueueIdVar(qid) / "clear" as _ =>
      se.clearQueue(inputQueue, qid) *>
        Ok(s"All sequences removed from queue $qid")

    case POST -> Root / "queue" / QueueIdVar(qid) / "run" / ObserverVar(observer) / ClientIDVar(
          clientId
        ) as user =>
      se.startQueue(inputQueue, qid, observer, user, clientId) *>
        Ok(s"Started queue $qid")

    case POST -> Root / "queue" / QueueIdVar(qid) / "stop" / ClientIDVar(clientId) as _ =>
      se.stopQueue(inputQueue, qid, clientId) *>
        Ok(s"Stopped from queue $qid")

    case POST -> Root / "execute" / ObsIdVar(oid) / PosIntVar(step) / ResourceVar(
          resource
        ) / ObserverVar(obs) / ClientIDVar(clientId) as u =>
      se.configSystem(inputQueue, oid, obs, u, step, resource, clientId) *>
        Ok(s"Run ${resource.show} from config at ${oid.format}/$step by ${u.username}/${obs.value}")

  }

  val refreshCommand: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "refresh" / ClientIDVar(clientId) =>
      se.requestRefresh(inputQueue, clientId) *> NoContent()

    case POST -> Root / "resetconditions" =>
      se.resetConditions(inputQueue) *> NoContent()
  }

  val service: HttpRoutes[F] =
    refreshCommand <+> TokenRefresher(GZip(httpAuthentication.reqAuth(commandServices)),
                                      httpAuthentication
    )
}
