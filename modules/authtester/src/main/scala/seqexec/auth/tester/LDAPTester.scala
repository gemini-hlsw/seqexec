// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import cats.effect._
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scopt.OParser

case class Config(host: String, port: Int, username: String, password: Option[String])

/**
 * Small utility to exercise the LDAP authentication service Build with `sbt "project authtester"
 * stage`
 *
 * Excute as: `./auth_tester -- -h <host> -o <port> -u <username> [-p <password>]`
 *
 * JAVA_HOME needs to be defined
 */
object Main extends IOApp {
  val builder = OParser.builder[Config]

  private implicit def logger: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("seqexec-engine")

  val parser = {
    import builder._
    OParser.sequence(
      programName("authtester"),
      head("authtester", "1.0"),
      opt[String]('h', "host")
        .required()
        .action((x, c) => c.copy(host = x))
        .text("host of the LDAP server"),
      opt[String]('u', "username")
        .required()
        .action((x, c) => c.copy(username = x))
        .text("username to test"),
      opt[String]('p', "password")
        .action((x, c) => c.copy(password = x.some))
        .text("password to test"),
      opt[Int]('o', "port")
        .required()
        .action((x, c) => c.copy(port = x))
        .text("port on the LADP server")
    )
  }

  def run(args: List[String]): IO[ExitCode] =
    OParser.parse(parser, args, Config("", -1, "", None)) match {
      case Some(config) =>
        val ldapService: AuthService[IO] = new FreeLDAPAuthenticationService(
          List((config.host, config.port))
        )
        val pwd                          = config.password match {
          case Some(s) => IO.pure(s)
          case None    =>
            IO.blocking(System.console().readPassword("Enter password: ").mkString)
        }
        pwd
          .flatMap(
            ldapService
              .authenticateUser(config.username, _)
          )
          .flatMap(r => IO.println(s"Result: $r"))
          .as(ExitCode(0))
      case _            =>
        // arguments are bad, error message will have been displayed
        IO(ExitCode(1))
    }
}
