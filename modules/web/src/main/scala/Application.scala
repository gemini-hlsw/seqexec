// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import gem.{ Service => GemService }
import gem.json._
import org.http4s._
import org.http4s.dsl._
// import scalaz._, Scalaz._
import scalaz.concurrent.Task

/**
 * The main application web service, which is "authenticated" in the sense that request carries
 * along a Service[Task] that provides access to the Gem back-end.
 */
object Application {

  // // String decoders
  // implicit val ProgramIdParamDecoder: QueryParamDecoder[Program.Id] =
  //   new QueryParamDecoder[Program.Id] {
  //     def decode(s: String) =
  //       Program.Id.fromString(s).toSuccessNel(ParseFailure("Invalid progam id.", s"Invalid progam id: $s"))
  //   }

  // These give us unapplies we can use for matching arguments.
  private val Query  = QueryParamDecoder[String].optMatcher("query")
  private val Limit  = QueryParamDecoder[Int].optMatcher("limit")
  // private val ProgId = QueryParamDecoder[Program.Id].optMatcher("pid")

  /** Turn a glob-style pattern into a SQL pattern. */
  def globToSql(s: String): String =
    s.replaceAll("\\*", "%")
     .replaceAll("\\.", "?")

  /** Gem application endpoints. */
  def service: AuthedService[GemService[Task]] =
    AuthedService {

      // Select matching program ids and titles.
      case GET -> Root / "api" / "query" / "program" :? Query(q) +& Limit(n) as gs =>
        val pattern = globToSql(q.getOrElse("*"))
        val limit   = n.getOrElse(100)
        gs.queryProgramsByName(pattern, limit).flatMap(ps => Ok(ps.map(p => (p.id, p.title))))

    }

}
