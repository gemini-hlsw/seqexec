// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl

import cats.implicits._, cats.effect.IO
import com.monovore.decline.{ Command => Cmd, _ }
import gem.ctl.free.ctl.{ CtlIO, Server, Host }
import gem.ctl.free.interpreter.Config
import gem.ctl.hi._

/** Command-line option parsers for gemctl. */
object Parsers {

  // Parsers for a Config object that provides global information to the interpreter.
  private object ConfigParsers {

    private lazy val verbose: Opts[Boolean] =
      Opts.flag(
        short = "v",
        long  = "verbose",
        help  = "Show details about what we're doing under the hood."
      ).orFalse

    private lazy val ansi: Opts[Boolean] =
      Opts.flag(
        long  = "no-ansi",
        help  = "Don't use ANSI colors and control codes."
      ).orTrue

    private lazy val host: Opts[Option[String]] =
      Opts.option[String](
        short   = "H",
        long    = "host",
        metavar = "HOST",
        help    = "Remote docker host."
      ).orNone

    private lazy val user: Opts[Option[String]] =
      Opts.option[String](
        short = "u",
        long  = "user",
        metavar = "USER",
        help = "Remote docker user (ignored unless -H is also specified)."
      ).orNone

    private lazy val server: Opts[Server] =
      (host, user) mapN {
        case (Some(h), ou) => Server.Remote(Host(h), ou)
        case (None,    _ ) => Server.Local
      }

    lazy val config: Opts[Config] =
      (verbose, server, ansi).mapN(Config)

  }

  // Parsers for individual commands.
  private object CommandParsers {

    private lazy val version: Opts[String] =
      Opts.argument[String](metavar = "VERSION")

    private lazy val deployTest: Opts[CtlIO[Unit]] =
      version.map(Deploy.deployTest(_).void)

    private lazy val deployProduction: Opts[CtlIO[Unit]] =
      version.map(Deploy.deployProduction(_).void)

    lazy val deployTestCommand: Opts[CtlIO[Unit]] =
      Opts.subcommand(
        name = "deploy-test",
        help = "Deploy gem, destroying any existing deployment."
      )(deployTest.widen[CtlIO[Unit]])

    lazy val deployProductionCommand: Opts[CtlIO[Unit]] =
      Opts.subcommand(
        name = "deploy-production",
        help = "Deploy gem, upgrading from any existing deployment, which will be shut down."
      )(deployProduction.widen[CtlIO[Unit]])

    lazy val impl: Opts[CtlIO[Unit]] =
      List(
        deployTestCommand,
        deployProductionCommand
      ).foldRight(Opts.never: Opts[CtlIO[Unit]])(_ orElse _)

  }

  // Top-level command parser
  private def top(progName: String): Cmd[(Config, CtlIO[Unit])] =
    Cmd(
      name   = progName,
      header = "Deploy and control gem."
    )((ConfigParsers.config, CommandParsers.impl).tupled)

  /**
  * Construct a program to parse commandline `args` into a `Command`, or show help information if
  * parsing fails. The `progName` argument is only used for help messages, and should match the
  * name of the executable ("gemctl" for example).
  */
  def parse[A](progName: String, args: List[String]): IO[Option[(Config, CtlIO[Unit])]] =
    top(progName).parse(args) match {
      case Right(c) => c.some.pure[IO]
      case Left(h) => IO(Console.println(Console.BLUE + h.toString + Console.RESET)).as(none[(Config, CtlIO[Unit])]) // scalastyle:ignore
    }

}