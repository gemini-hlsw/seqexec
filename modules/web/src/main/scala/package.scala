// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import argonaut._
import org.http4s._
import org.http4s.argonaut._
import org.http4s.dsl._
import scalaz._, Scalaz._

package object web {

  implicit val FunctorQueryParamDecoder: Functor[QueryParamDecoder] =
    new Functor[QueryParamDecoder] {
      def map[A, B](fa: QueryParamDecoder[A])(f: A => B): QueryParamDecoder[B] =
        QueryParamDecoder.decodeBy(f)(fa)
    }

  implicit class QueryParamDecoderOps[A](fa: QueryParamDecoder[A]) {
    def matcher(key: String): QueryParamDecoderMatcher[A] =
      new QueryParamDecoderMatcher[A](key)(fa) {}
    def optMatcher(key: String): OptionalQueryParamDecoderMatcher[A] =
      new OptionalQueryParamDecoderMatcher[A](key)(fa) {}
  }

  implicit class RequestOps(val req: Request) extends AnyVal {
    def findCookie(name: String): Option[Cookie] =
      headers.Cookie.from(req.headers).flatMap(cs => cs.values.list.find(_.name === name))
  }

  // Anything that can be swizzled with Json can be sent or received.
  implicit def automaticJsonDecoderOf[A: DecodeJson]: EntityDecoder[A] = jsonOf
  implicit def automaticJsonEncoderOf[A: EncodeJson]: EntityEncoder[A] = jsonEncoderOf

}
