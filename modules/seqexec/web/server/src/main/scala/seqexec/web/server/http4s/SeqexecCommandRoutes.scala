// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.IO
import gem.Observation
import seqexec.server.Commands
import seqexec.server.SeqexecEngine
import seqexec.server
import seqexec.model.enum.{ CloudCover, ImageQuality, SkyBackground, WaterVapor}
import seqexec.model._
import seqexec.web.server.model.CommandsModel._
import seqexec.web.server.http4s.encoder._
import seqexec.web.server.security.{AuthenticationService, Http4sAuthentication, TokenRefresher}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server.middleware.GZip
import cats.implicits._

/**
  * Rest Endpoints under the /api route
  */
class SeqexecCommandRoutes(auth: AuthenticationService, inputQueue: server.EventQueue, se: SeqexecEngine) extends BooEncoders {

  private val commands = Commands(se.odbProxy)
  // Handles authentication
  private val httpAuthentication = new Http4sAuthentication(auth)

  private val commandServices: AuthedService[UserDetails, IO] = AuthedService {
    case GET  -> Root  / "host" as _ =>
      Ok(toCommandResult("host", commands.host()))

    case GET  -> Root  / obsId / "count" as _ =>
      Ok(toCommandResult("count", commands.showCount(obsId)))

    case POST -> Root / ObsIdVar(obsId) / "start" / ClientIDVar(clientId) as user =>
      for {
        _     <- se.start(inputQueue, obsId, user, clientId)
        resp  <- Ok(s"Started sequence $obsId")
      } yield resp

    case POST -> Root / ObsIdVar(obsId) / "pause" as user =>
      for {
        _     <- se.requestPause(inputQueue, obsId, user)
        resp  <- Ok(s"Pause sequence $obsId")
      } yield resp

   case POST -> Root / ObsIdVar(obsId) / "cancelpause" as user =>
     for {
       _     <- se.requestCancelPause(inputQueue, obsId, user)
       resp  <- Ok(s"Cancel Pause sequence $obsId")
     } yield resp

   case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "breakpoint" / BooleanVar(bp) as user =>
     for {
       _      <- se.setBreakpoint(inputQueue, obsId, user, stepId, bp)
       resp   <- Ok(s"Set breakpoint in step $stepId of sequence $obsId")
     } yield resp

    case GET -> Root / ObsIdVar(obsId) / "sync" as _ =>
      for {
        u     <- se.load(inputQueue, obsId)
        resp  <- u.fold(_ => NotFound(s"Not found sequence $obsId"), _ =>
          Ok(SequencesQueue[Observation.Id](Map.empty, Conditions.Default, None, List(obsId))))
      } yield resp

   case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "skip" / bp as user =>
     for {
       newVal <- IO.fromEither(Either.catchNonFatal(bp.toBoolean))
       _      <- se.setSkipMark(inputQueue, obsId, user, stepId, newVal)
       resp   <- Ok(s"Set skip mark in step $stepId of sequence $obsId")

     } yield resp

   case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "stop" as _ =>
     for {
       _    <- se.stopObserve(inputQueue, obsId)
       resp <- Ok(s"Stop requested for $obsId on step $stepId")
     } yield resp

   case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "abort" as _ =>
     for {
       _    <- se.abortObserve(inputQueue, obsId)
       resp <- Ok(s"Abort requested for $obsId on step $stepId")
     } yield resp

   case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "pauseObs" as _ =>
     for {
       _    <- se.pauseObserve(inputQueue, obsId)
       resp <- Ok(s"Pause observation requested for $obsId on step $stepId")
     } yield resp

   case POST -> Root / ObsIdVar(obsId) / PosIntVar(stepId) / "resumeObs" as _ =>
     for {
       _    <- se.resumeObserve(inputQueue, obsId)
       resp <- Ok(s"Resume observation requested for $obsId on step $stepId")
     } yield resp

   case POST -> Root / "operator" / OperatorVar(op) as user =>
     se.setOperator(inputQueue, user, op) *> Ok(s"Set operator name to '${op.value}'")

   case POST -> Root / ObsIdVar(obsId) / "observer" / ObserverVar(obs) as user =>
     for {
       _     <- se.setObserver(inputQueue, obsId, user, obs)
       resp  <- Ok(s"Set observer name to '${obs.value}' for sequence $obsId")
     } yield resp

    case req @ POST -> Root / "conditions" as user =>
      req.req.decode[Conditions] (conditions =>
        se.setConditions(inputQueue, conditions, user) *> Ok(s"Set conditions to $conditions")
      )

    case req @ POST -> Root / "iq" as user =>
      req.req.decode[ImageQuality] (iq =>
        se.setImageQuality(inputQueue, iq, user) *> Ok(s"Set image quality to $iq")
      )

    case req @ POST -> Root / "wv" as user =>
      req.req.decode[WaterVapor] (wv =>
        se.setWaterVapor(inputQueue, wv, user) *> Ok(s"Set water vapor to $wv")
      )

    case req @ POST -> Root / "sb" as user =>
      req.req.decode[SkyBackground] (sb =>
        se.setSkyBackground(inputQueue, sb, user) *> Ok(s"Set sky background to $sb")
      )

    case req @ POST -> Root / "cc" as user =>
      req.req.decode[CloudCover] (cc =>
        se.setCloudCover(inputQueue, cc, user) *> Ok(s"Set cloud cover to $cc")
      )

    case POST -> Root / "load" / InstrumentVar(i) / ObsIdVar(obsId) / ObserverVar(observer) as user =>
      se.loadSequence(inputQueue, i, obsId, observer, user) *> Ok(s"Set selected sequence $obsId for $i")

    case POST -> Root / "unload" / "all" as user =>
      se.clearLoadedSequences(inputQueue, user) *> Ok(s"Queue cleared")
  }

  val refreshCommand: HttpService[IO] = HttpService[IO] {
    case GET -> Root / "refresh" / ClientIDVar(clientId) =>
      se.requestRefresh(inputQueue, clientId) *> NoContent()
  }

  val service: HttpService[IO] = refreshCommand <+> TokenRefresher(GZip(httpAuthentication.reqAuth(commandServices)), httpAuthentication)
}
