package edu.gemini.seqexec.web.server.http4s

import edu.gemini.seqexec.web.server.http4s.TokenAuthenticator.TokenExtractor
import org.http4s.{Service, _}

import scalaz.concurrent.Task
import org.http4s.server._

import scalaz._
import Scalaz._

object TokenAuthenticator {
  /**
   * TokenAuthentication function that extract a token from the request, e.g.
   * from a Cookie or a header
   */
  type TokenExtractor = Request => Task[Option[String]]

}

trait TokenInCookies {
  // Can be overriden for custom cookie name
  val cookieName: String = "token"

  val extractor: TokenExtractor = req => Task.now {
    for {
      cookies <- req.headers.get(headers.`Cookie`).map(_.values)
      token   <- cookies.findLeft(_.name == cookieName)
    } yield token.content
  }
}

/**
 * TokenAuthentication instances are middleware that provide a
 * {@link HttpService} with optional HTTP authentication.
 *
 * @type A user object type
 */
trait TokenAuthenticator[A] extends HttpMiddleware {
  type TokenAuth = String => Task[Option[A]]

  val store: TokenAuth
  val extractor: TokenExtractor
  val attributeKey: AttributeKey[Option[A]]

  /**
   * Check if req contains valid credentials. You may assume that
   * the returned Task is executed at most once (to allow for side-effects,
   * e.g. the incrementation of a nonce counter in DigestAuthentication).
    *
    * @param req The request received from the client.
   * @return If req contains valid credentials, a copy of req is returned that
   *         contains additional attributes pertaining to authentication such
   *         as the username and realm from the valid credentials.
   *         If req does not contain valid credentials, a challenge is returned.
   *         This challenge will be included in the HTTP 401
   *         Unauthorized response that is returned to the client.
   *
   */
  protected def getUser(req: Request): Task[Option[A]] =
    extractor(req).flatMap {
      case Some(t) => store(t)
      case None    => Task.now(None)
    }

  def apply(service: HttpService): HttpService = Service.lift { req =>
    getUser(req) flatMap {
      case u @ Some(_) =>
        service(req.withAttribute(attributeKey, u))
      case None =>
        service(req)
    }
  }
}


