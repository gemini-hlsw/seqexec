package edu.gemini.seqexec.web.server.http4s

import edu.gemini.seqexec.web.common.UserDetails
import edu.gemini.seqexec.web.server.security.{AuthenticationConfig, MissingCookie}
import edu.gemini.seqexec.web.server.security.AuthenticationService._
import org.http4s.{AttributeKey, Challenge, Request, headers}
import org.http4s.server.middleware.authentication.Authentication

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object JwtAuthentication {
  val authenticatedUser = AttributeKey[UserDetails]("edu.gemini.seqexec.authenticatedUser")
}

class JwtAuthentication extends Authentication {
  override protected def getChallenge(req: Request): Task[\/[Challenge, Request]] = {
    val challenge = -\/(Challenge("jwt", Realm))

    def checkAuth(req: Request): Task[AuthResult] = Task.now {
      for {
        cookies <- req.headers.get(headers.`Cookie`).map(_.values) \/> MissingCookie
        token   <- cookies.findLeft(_.name == AuthenticationConfig.cookieName) \/> MissingCookie
        auth    <- decodeToken2(token.content)
      } yield auth
    }

    checkAuth(req) >>= {
      case \/-(u) => Task.now(\/-(req.withAttribute(JwtAuthentication.authenticatedUser, u)))
      case -\/(e) => Task.now(challenge)
    }
  }
}

