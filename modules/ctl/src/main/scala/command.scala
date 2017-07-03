// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl

import com.monovore.decline.{ Command => Cmd, _ }

import cats.implicits._, cats.effect.IO

import gem.ctl.free.ctl.{ Server, Host }
import gem.ctl.free.interpreter.Config

/** A command to be interpreted by gemctl. */
sealed trait Command extends Config

/** Module of constructors of `Command`, and a parser too. */
object Command {

  final case class Deploy(
    server: Server,
    deployRev:   String,
    standalone:  Boolean,
    verbose:     Boolean,
    force:       Boolean
  ) extends Command

  final case class Ps  (    server: Server, verbose: Boolean) extends Command
  final case class Stop(    server: Server, verbose: Boolean) extends Command
  final case class Log (    server: Server, verbose: Boolean, count: Int) extends Command
  final case class Rollback(server: Server, verbose: Boolean) extends Command

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

  // Opts implementation below

  private lazy val machine: Opts[Boolean] =
    Opts.flag(
      short = "m",
      long  = "machine",
      help  = "Use docker machine. Use with -H to specify a machine other than 'default'."
    ).orFalse

  private lazy val host: Opts[Option[String]] =
    Opts.option[String](
      short   = "H",
      long    = "host",
      metavar = "HOST",
      help    = "Use the specified docker host (machine when given with -m)."
    ).orNone

  private lazy val server: Opts[Server] =
    (machine |@| host |@| user) map {
      case (true,  oh,      ou) => Server.Remote(Host.Machine(oh.getOrElse("default")), ou)
      case (false, Some(h), ou) => Server.Remote(Host.Network(h), ou)
      case (false, None,    _ ) => Server.Local
    }

  private lazy val user: Opts[Option[String]] =
    Opts.option[String](
      short = "u",
      long  = "user",
      metavar = "USER",
      help = "Server user. Default value is 'docker' with -m, otherwise current user, ignored if neither -H nor -m is specified."
    ).orNone

  private lazy val deployRevision: Opts[String] =
    Opts.option[String](
      short   = "d",
      long    = "deploy",
      metavar = "REVISION",
      help    = "Revision to deploy, HEAD if unspecified."
    ).withDefault("HEAD")

  private lazy val standalone: Opts[Boolean] =
    Opts.flag(
      short = "s",
      long  = "standalone",
      help  = "Deploy standalone; do not attempt an upgrade. Cannot be specified with --base"
    ).orFalse

  private lazy val force: Opts[Boolean] =
    Opts.flag(
      short = "f",
      long  = "force",
      help  = "Force an upgrade, even if base and deploy revisions are identical."
    ).orFalse

  private lazy val verbose: Opts[Boolean] =
    Opts.flag(
      short = "v",
      long  = "verbose",
      help  = "Show details about what we're doing under the hood."
    ).orFalse

  private lazy val deploy: Opts[Deploy] =
    (server |@| deployRevision |@| standalone |@| verbose |@| force) map Deploy.apply

  private lazy val lines: Opts[Int] = {
    val DefaultLines = 50
    Opts.option[Int](
      short   = "n",
      long    = "lines",
      metavar = "LINES",
      help    = s"Number of lines to show from tail of log (default $DefaultLines)"
    ).withDefault(DefaultLines)
  }

  private lazy val configCommand: Opts[Command] =
    Opts.subcommand(
      name = "deploy",
      help = "Deploy an application."
    )(deploy.widen[Command])

  private lazy val psCommand: Opts[Command] =
    Opts.subcommand(
      name = "ps",
      help = "Get the status of a gem deployment."
    )((server |@| verbose).map(Ps).widen[Command])

  private lazy val stopCommand: Opts[Command] =
    Opts.subcommand(
      name = "stop",
      help = "Stop a gem deployment."
    )((server |@| verbose).map(Stop).widen[Command])

  private lazy val logCommand: Opts[Command] =
    Opts.subcommand(
      name = "log",
      help = "Show the Gem server log."
    )((server |@| verbose |@| lines).map(Log).widen[Command])

  private lazy val rollbackCommand: Opts[Command] =
    Opts.subcommand(
      name = "rollback",
      help = "Roll the current gem deployment back to the previous one, if possible."
    )((server |@| verbose).map(Rollback).widen[Command])

  private def mainParser(progName: String): Cmd[Command] =
    Cmd(
      name   = progName,
      header = "Deploy and control gem."
    )(List(configCommand, psCommand, stopCommand, logCommand, rollbackCommand).foldRight(Opts.never: Opts[Command])(_ orElse _))

}
