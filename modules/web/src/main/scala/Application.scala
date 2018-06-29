// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import cats.effect.IO
import gem.{ Service => GemService }
import gem.json.instances.all._
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._

/**
 * The main application web service, which is "authenticated" in the sense that request carries
 * along a Service[IO] that provides access to the Gem back-end.
 */
object Application {

  // These give us unapplies we can use for matching arguments.
  private val Query  = QueryParamDecoder[String].optMatcher("query")
  private val Limit  = QueryParamDecoder[Int].optMatcher("limit")

  /** Turn a glob-style pattern into a SQL pattern. */
  def globToSql(s: String): String =
    s.replaceAll("\\*", "%")
     .replaceAll("\\.", "?")

  /** Gem application endpoints. */
  def service: AuthedService[GemService[IO], IO] =
    AuthedService {

      // Select matching program ids and titles.
      case GET -> Root / "api" / "query" / "program" :? Query(q) +& Limit(n) as gs =>
        val pattern = globToSql(q.getOrElse("*"))
        val limit   = n.getOrElse(100)
        gs.queryProgramsByName(pattern, limit).flatMap(ps => Ok(ps.map(p => (p._1, p._2)).asJson ))

    }

}
