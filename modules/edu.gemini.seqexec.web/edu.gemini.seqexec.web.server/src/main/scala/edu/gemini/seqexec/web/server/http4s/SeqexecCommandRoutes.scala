package edu.gemini.seqexec.web.server.http4s

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.Commands
import edu.gemini.seqexec.server.SeqexecEngine
import edu.gemini.seqexec.server
import edu.gemini.seqexec.model.Model.{CloudCover, Conditions, ImageQuality, SkyBackground, WaterVapor}
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.server.model.CommandsModel._
import edu.gemini.seqexec.web.server.http4s.encoder._
import edu.gemini.seqexec.web.server.security.{AuthenticationService, Http4sAuthentication, TokenRefresher}
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.syntax._
import org.http4s.server.middleware.GZip
import org.http4s.server.AuthMiddleware

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

/**
  * Rest Endpoints under the /api route
  */
class SeqexecCommandRoutes(auth: AuthenticationService, inputQueue: server.EventQueue, se: SeqexecEngine) extends BooEncoders {

  val commands = Commands(se.odbProxy)
  // Handles authentication
  val httpAuthentication = new Http4sAuthentication(auth)

  val commandServices: AuthedService[UserDetails] = AuthedService {
    case GET  -> Root  / "host" as _ =>
      Ok(toCommandResult("host", commands.host()))

    case GET  -> Root  / obsId / "count" as _ =>
      Ok(toCommandResult("count", commands.showCount(obsId)))

    case POST -> Root / obsId / "start" as _ =>
      for {
        obs <-
            \/.fromTryCatchNonFatal(new SPObservationID(obsId))
              .fold(e => Task.fail(e), Task.now)
        _     <- se.start(inputQueue, obs)
        resp  <- Ok(s"Started sequence $obs")
      } yield resp

    case POST -> Root / obsId / "pause" as _ =>
      for {
        obs <-
            \/.fromTryCatchNonFatal(new SPObservationID(obsId))
              .fold(e => Task.fail(e), Task.now)
        _     <- se.requestPause(inputQueue, obs)
        resp  <- Ok(s"Pause sequence $obs")
      } yield resp

    case POST -> Root / obsId / stepId / "breakpoint" / bp as _ =>
      for {
        obs    <- \/.fromTryCatchNonFatal(new SPObservationID(obsId)).fold(e => Task.fail(e), Task.now)
        step   <- \/.fromTryCatchNonFatal(stepId.toInt).fold(e => Task.fail(e), Task.now)
        newVal <- \/.fromTryCatchNonFatal(bp.toBoolean).fold(e => Task.fail(e), Task.now)
        _      <- se.setBreakpoint(inputQueue, obs, step, newVal)
        resp   <- Ok(s"Set breakpoint in step $step of sequence $obsId")

      } yield resp

    case POST -> Root / "operator" / name as _ =>
      se.setOperator(inputQueue, name) *> Ok(s"Set operator name to $name")

    case POST -> Root / obsId / "observer" / name as _ =>
      for {
        obs   <-
          \/.fromTryCatchNonFatal(new SPObservationID(obsId))
            .fold(e => Task.fail(e), Task.now)
        _     <- se.setObserver(inputQueue, obs, name)
        resp  <- Ok(s"Set observer name to $name for sequence $obs")
      } yield resp


    case req @ POST -> Root / "conditions" as _ =>
      req.req.decode[Conditions] (conditions =>
        se.setConditions(inputQueue, conditions) *> Ok(s"Set conditions to $conditions")
      )

    case req @ POST -> Root / "iq" as _ =>
      req.req.decode[ImageQuality] (iq =>
        se.setImageQuality(inputQueue, iq) *> Ok(s"Set image quality to $iq")
      )

    case req @ POST -> Root / "wv" as _ =>
      req.req.decode[WaterVapor] (wv =>
        se.setWaterVapor(inputQueue, wv) *> Ok(s"Set water vapor to $wv")
      )

    case req @ POST -> Root / "sb" as _ =>
      req.req.decode[SkyBackground] (sb =>
        se.setSkyBackground(inputQueue, sb) *> Ok(s"Set sky background to $sb")
      )

    case req @ POST -> Root / "cc" as _ =>
      req.req.decode[CloudCover] (cc =>
        se.setCloudCover(inputQueue, cc) *> Ok(s"Set cloud cover to $cc")
      )

    }

  val refreshCommand: HttpService = HttpService {
    case GET -> Root / "refresh" =>
      se.requestRefresh(inputQueue) *> NoContent()

  }

  val service: Service[Request, MaybeResponse] = refreshCommand |+| TokenRefresher(httpAuthentication, GZip(httpAuthentication.reqAuth(commandServices)))
}
