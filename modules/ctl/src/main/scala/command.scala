// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl

import cats.implicits._, cats.effect.IO
import com.monovore.decline.{ Command => Cmd, _ }
import gem.ctl.free.ctl.{ CtlIO, Server, Host }
import gem.ctl.free.interpreter.Config
import gem.ctl.hi._

/** A command to be interpreted by gemctl. */
sealed trait Command extends Config {
  def impl: CtlIO[Unit]
}

/** Module of constructors of `Command`, and a parser too. */
object Command {

  final case class DeployTest(
    verbose: Boolean,
    server:  Server,
    version: String
  ) extends Command {
    val impl = Deploy.deployTest(version).void
  }

  final case class DeployProduction(
    verbose: Boolean,
    server:  Server,
    version: String
  ) extends Command {
    val impl = Deploy.deployProduction(version).void
  }

  /**
   * Construct a program to parse commandline `args` into a `Command`, or show help information if
   * parsing fails. The `progName` argument is only used for help messages, and should match the
   * name of the executable ("gemctl" for example).
   */
  def parse[A](progName: String, args: List[String]): IO[Option[Command]] =
    mainParser(progName).parse(args) match {
      case Right(c) => c.some.pure[IO]
      case Left(h) => IO(Console.println(Console.BLUE + h.toString + Console.RESET)).as(none[Command]) // scalastyle:ignore
    }

  private lazy val host: Opts[Option[String]] =
    Opts.option[String](
      short   = "H",
      long    = "host",
      metavar = "HOST",
      help    = "Remote docker host."
    ).orNone

  private lazy val server: Opts[Server] =
    (host, user) mapN {
      case (Some(h), ou) => Server.Remote(Host(h), ou)
      case (None,    _ ) => Server.Local
    }

  private lazy val user: Opts[Option[String]] =
    Opts.option[String](
      short = "u",
      long  = "user",
      metavar = "USER",
      help = "Remote docker user (ignored unless -H is also specified)."
    ).orNone

  private lazy val version: Opts[String] =
    Opts.argument[String](metavar = "VERSION")

  private lazy val verbose: Opts[Boolean] =
    Opts.flag(
      short = "v",
      long  = "verbose",
      help  = "Show details about what we're doing under the hood."
    ).orFalse

  private lazy val deployTest: Opts[DeployTest] =
    (verbose, server, version) mapN DeployTest.apply

  private lazy val deployTestCommand: Opts[Command] =
    Opts.subcommand(
      name = "deploy-test",
      help = "Deploy gem, destroying any existing deployment."
    )(deployTest.widen[Command])

  private lazy val deployProduction: Opts[DeployProduction] =
    (verbose, server, version) mapN DeployProduction.apply

  private lazy val deployProductionCommand: Opts[Command] =
    Opts.subcommand(
      name = "deploy-production",
      help = "Deploy gem, upgrading from any existing deployment, which will be shut down."
    )(deployProduction.widen[Command])

  private def mainParser(progName: String): Cmd[Command] =
    Cmd(
      name   = progName,
      header = "Deploy and control gem."
    )(List(deployTestCommand, deployProductionCommand).foldRight(Opts.never: Opts[Command])(_ orElse _))

}
