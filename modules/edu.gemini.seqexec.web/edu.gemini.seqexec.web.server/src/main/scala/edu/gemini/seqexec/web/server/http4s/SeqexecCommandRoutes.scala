// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server.http4s

import cats.effect.IO
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.Commands
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.server
import edu.gemini.seqexec.model.Model.{ClientID, CloudCover, Conditions, ImageQuality, Observer, Operator, SkyBackground, WaterVapor}
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.server.model.CommandsModel._
import edu.gemini.seqexec.web.server.http4s.encoder._
import edu.gemini.seqexec.web.server.security.{AuthenticationService, Http4sAuthentication, TokenRefresher}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server.middleware.GZip
import cats.implicits._

object ClientIDVar {
  def unapply(str: String): Option[ClientID] =
    Either.catchNonFatal(java.util.UUID.fromString(str)).toOption
}
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

    case POST -> Root / obsId / "start" / ClientIDVar(clientId) as user =>
      for {
        obs   <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
        _     <- se.start(inputQueue, obs, user, clientId)
        resp  <- Ok(s"Started sequence $obs")
      } yield resp

    case POST -> Root / obsId / "pause" as user =>
      for {
        obs   <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
        _     <- se.requestPause(inputQueue, obs, user)
        resp  <- Ok(s"Pause sequence $obs")
      } yield resp

   case POST -> Root / obsId / "cancelpause" as user =>
     for {
       obs   <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
       _     <- se.requestCancelPause(inputQueue, obs, user)
       resp  <- Ok(s"Cancel Pause sequence $obs")
     } yield resp

   case POST -> Root / obsId / stepId / "breakpoint" / bp as user =>
     for {
       obs    <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
       step   <- IO.fromEither(Either.catchNonFatal(stepId.toInt))
       newVal <- IO.fromEither(Either.catchNonFatal(bp.toBoolean))
       _      <- se.setBreakpoint(inputQueue, obs, user, step, newVal)
       resp   <- Ok(s"Set breakpoint in step $step of sequence $obsId")

     } yield resp

   case POST -> Root / obsId / stepId / "skip" / bp as user =>
     for {
       obs    <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
       step   <- IO.fromEither(Either.catchNonFatal(stepId.toInt))
       newVal <- IO.fromEither(Either.catchNonFatal(bp.toBoolean))
       _      <- se.setSkipMark(inputQueue, obs, user, step, newVal)
       resp   <- Ok(s"Set skip mark in step $step of sequence $obsId")

     } yield resp

   case POST -> Root / obsId / stepId / "stop" as _ =>
     for {
       obs  <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
       _    <- se.stopObserve(inputQueue, obs)
       resp <- Ok(s"Stop requested for $obsId on step $stepId")
     } yield resp

   case POST -> Root / obsId / stepId / "abort" as _ =>
     for {
       obs  <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
       _    <- se.abortObserve(inputQueue, obs)
       resp <- Ok(s"Abort requested for $obsId on step $stepId")
     } yield resp

   case POST -> Root / obsId / stepId / "pauseObs" as _ =>
     for {
       obs  <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
       _    <- se.pauseObserve(inputQueue, obs)
       resp <- Ok(s"Pause observation requested for $obsId on step $stepId")
     } yield resp

   case POST -> Root / obsId / stepId / "resumeObs" as _ =>
     for {
       obs  <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
       _    <- se.resumeObserve(inputQueue, obs)
       resp <- Ok(s"Resume observation requested for $obsId on step $stepId")
     } yield resp

   case POST -> Root / "operator" / name as user =>
     se.setOperator(inputQueue, user, Operator(name)) *> Ok(s"Set operator name to '$name'")

   case POST -> Root / obsId / "observer" / name as user =>
     for {
       obs   <- IO.fromEither(Either.catchNonFatal(new SPObservationID(obsId)))
       _     <- se.setObserver(inputQueue, obs, user, Observer(name))
       resp  <- Ok(s"Set observer name to '$name' for sequence $obs")
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

    }

  val refreshCommand: HttpService[IO] = HttpService[IO] {
    case GET -> Root / "refresh" / ClientIDVar(clientId) =>
      se.requestRefresh(inputQueue, clientId) *> NoContent()
  }

  val service: HttpService[IO] = refreshCommand <+> TokenRefresher(GZip(httpAuthentication.reqAuth(commandServices)), httpAuthentication)
}
