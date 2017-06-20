// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl

import net.bmjames.opts._
import net.bmjames.opts.types.{ Success, Failure }
import net.bmjames.opts.builder.internal.{ Mod, CommandFields }

import scalaz._, Scalaz._, scalaz.effect._

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
    execParserPure(prefs(idm[PrefsMod]), mainParser, args) match {
      case Success(c) => IO(Some(c))
      case Failure(f) =>
        import Predef._
        val (msg, exit) = renderFailure(f, progName)
        IO.putStr(Console.BLUE) *>
        IO.putStrLn(msg) *>
        IO.putStrLn("")  *>
        IO.putStrLn(s"""
          |Hints:
          |  $progName --help            To see available commands.
          |  $progName COMMAND --help    To see help on a specific command.
         """.trim.stripMargin) *>
         IO.putStr(Console.RESET) as None
    }

  // Parser implementation below

  private lazy val machine: Parser[Boolean] =
    switch(
      short('m'), long("machine"),
      help("Use docker machine. Use with -H to specify a machine other than 'default'.")
    )

  private lazy val host: Parser[Option[String]] =
    optional(
      strOption(
        short('H'), long("host"), metavar("HOST"),
        help("Use the specified docker host (machine when given with -m).")
      )
    )

  private lazy val server: Parser[Server] =
    (machine |@| host |@| user) {
      case (true,  oh,      ou) => Server.Remote(Host.Machine(oh.getOrElse("default")), ou)
      case (false, Some(h), ou) => Server.Remote(Host.Network(h), ou)
      case (false, None,    _ ) => Server.Local
    }

  private lazy val user: Parser[Option[String]] =
    optional(
      strOption(
        short('u'), long("user"), metavar("USER"),
        help("Server user. Default value is 'docker' with -m, otherwise current user, ignored if neither -H nor -m is specified.")
      )
    )

  private lazy val deployRevision: Parser[String] =
    strOption(
      short('d'), long("deploy"), metavar("REVISION"), value("HEAD"),
      help("Revision to deploy, HEAD if unspecified.")
    )

  private lazy val standalone: Parser[Boolean] =
    switch(
      short('s'), long("standalone"),
      help("Deploy standalone; do not attempt an upgrade. Cannot be specified with --base")
    )

  private lazy val force: Parser[Boolean] =
    switch(
      short('f'), long("force"),
      help("Force an upgrade, even if base and deploy revisions are identical.")
    )

  private lazy val verbose: Parser[Boolean] =
    switch(
      short('v'), long("verbose"),
      help("Show details about what we're doing under the hood.")
    )

  private lazy val deploy: Parser[Deploy] =
    (server |@| deployRevision |@| standalone |@| verbose |@| force)(Deploy.apply)

  private lazy val lines: Parser[Int] = {
    val DefaultLines = 50
    intOption(
      short('n'), metavar("LINES"), value(DefaultLines),
      help(s"Number of lines to show from tail of log (default $DefaultLines)")
    )
  }

  private lazy val configCommand: Mod[CommandFields, Command] =
    command("deploy", info(deploy.widen[Command] <* helper,
      progDesc("Deploy an application."))
    )

  private lazy val psCommand: Mod[CommandFields, Command] =
    command("ps", info((server |@| verbose)(Ps).widen[Command] <* helper,
      progDesc("Get the status of a gem deployment."))
    )

  private lazy val stopCommand: Mod[CommandFields, Command] =
    command("stop", info((server |@| verbose)(Stop).widen[Command] <* helper,
      progDesc("Stop a gem deployment."))
    )

  private lazy val logCommand: Mod[CommandFields, Command] =
    command("log", info((server |@| verbose |@| lines)(Log).widen[Command] <* helper,
      progDesc("Show the Gem server log."))
    )

  private lazy val rollbackCommand: Mod[CommandFields, Command] =
    command("rollback", info((server |@| verbose)(Rollback).widen[Command] <* helper,
      progDesc("Roll the current gem deployment back to the previous one, if possible."))
    )

  private lazy val mainParser: ParserInfo[Command] =
    info(
      subparser(configCommand, psCommand, stopCommand, logCommand, rollbackCommand) <*>
      helper, progDesc("Deploy and control gem."))

}
