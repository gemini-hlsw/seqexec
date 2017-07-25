// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import doobie.imports._
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.blaze._
import scalaz.concurrent._

object Main extends TaskApp {

  val xa: Transactor[Task, _] =
    DriverManagerTransactor[Task](
      "org.postgresql.Driver",
      "jdbc:postgresql:gem",
      "postgres",
      ""
    )

  def service(log: Log[Task]): HttpService =
    HttpService.lift {
      case req =>
        gem.Service.tryLogin("root", "", xa, log).flatMap {
          case None    => Forbidden()
          case Some(s) => WebService(s).run(req)
        }
    }

  def builder(log: Log[Task]): BlazeBuilder =
    BlazeBuilder
      .bindHttp(8080, "localhost")
      .mountService(service(log), "/")

  override def runl(args: List[String]): Task[Unit] =
    for {
      log <- Log.newLog[Task]("web", xa)
      svr <- builder(log).start
      _   <- Task.delay(Console.println("Press a key to exit."))
      _   <- Task.delay(io.StdIn.readLine())
      _   <- svr.shutdown
      _   <- log.shutdown(1000L)
    } yield ()

}
