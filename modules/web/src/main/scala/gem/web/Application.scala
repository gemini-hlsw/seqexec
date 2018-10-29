// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import cats._
import cats.implicits._
import gem.{ Service => GemService }
import gem.json.instances.all._
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.dsl._
import org.http4s.circe._

/**
 * The main application web service, which is "authenticated" in the sense that request carries
 * along a Service[F] that provides access to the Gem back-end.
 */
object Application {

  // These give us unapplies we can use for matching arguments.
  private val Query = QueryParamDecoder[String].optMatcher("query")
  private val Limit = QueryParamDecoder[Int].optMatcher("limit")
  private val Host  = QueryParamDecoder[String].matcher("host")

  /** Turn a glob-style pattern into a SQL pattern. */
  def globToSql(s: String): String =
    s.replaceAll("\\*", "%")
     .replaceAll("\\.", "?")

  def withObsId[F[_]: Monad](s: String)(f: Observation.Id => F[Response[F]]): F[Response[F]] = {
    val dsl = new Http4sDsl[F] {}; import dsl._
    Observation.Id.fromString(s).fold(BadRequest(s"Not an observation id: '$s'"))(f)
  }

  /** Gem application endpoints. */
  def service[F[_]: Monad]: AuthedService[GemService[F], F] = {
    val dsl = new Http4sDsl[F] {}; import dsl._
    AuthedService {

      // Select matching program ids and titles.
      case GET -> Root / "api" / "query" / "program" :? Query(q) +& Limit(n) as gs =>
        val pattern = globToSql(q.getOrElse("*"))
        val limit   = n.getOrElse(100)
        gs.queryProgramsByName(pattern, limit).flatMap(ps => Ok(ps.map(p => (p._1, p._2)).asJson ))

      // Import an observation from the legacy ODB.
      case GET -> Root / "api" / "import" / "obs" / o :? Host(h) as gs =>
        withObsId(o) { oid =>
          gs.ocs2.importObservation(h, oid).flatMap {
            _.fold(msg => InternalServerError(msg), _ => NoContent())
          }
        }

      // Fetch an observation by id.
      case GET -> Root / "api" / "fetch" / "obs" / o as gs =>
        withObsId(o) { oid =>
          gs.fetchObservationById(oid).flatMap { obs => Ok(obs.asJson) }
        }

      // Query an observation by id.
      case GET -> Root / "api" / "query" / "obs" / o as gs =>
        withObsId(o) { oid =>
          gs.queryObservationById(oid).flatMap { o =>
            o.fold(NotFound())(obs => Ok(obs.asJson))
          }
        }
    }
  }

}
