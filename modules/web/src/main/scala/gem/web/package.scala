// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.implicits._
import org.http4s._
import org.http4s.dsl.io._

package object web {

  implicit class QueryParamDecoderOps[A](fa: QueryParamDecoder[A]) {
    def matcher(key: String): QueryParamDecoderMatcher[A] =
      new QueryParamDecoderMatcher[A](key)(fa) {}
    def optMatcher(key: String): OptionalQueryParamDecoderMatcher[A] =
      new OptionalQueryParamDecoderMatcher[A](key)(fa) {}
  }

  implicit class RequestOps[F[_]](val req: Request[F]) extends AnyVal {
    def findCookie(name: String): Option[RequestCookie] =
      headers.Cookie.from(req.headers).flatMap(cs => cs.values.toList.find(_.name === name))
  }

}
