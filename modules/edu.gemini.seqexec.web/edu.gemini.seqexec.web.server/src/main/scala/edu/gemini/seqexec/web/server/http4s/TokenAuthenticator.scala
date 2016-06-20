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
 * with optional HTTP authentication.
 */
trait TokenAuthenticator[A] extends HttpMiddleware {
  type TokenAuth = String => Task[Option[A]]

  val store: TokenAuth
  val extractor: TokenExtractor
  val attributeKey: AttributeKey[Option[A]]

  /**
   * Check if req contains valid credentials. it delegates to TokenExtractor
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
      case None        =>
        service(req) // The request is allowed for an anonymous usel
    }
  }
}


