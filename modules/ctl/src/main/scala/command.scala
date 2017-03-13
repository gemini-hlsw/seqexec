package gem.ctl

import net.bmjames.opts._
import net.bmjames.opts.types.{ Success, Failure }
import net.bmjames.opts.builder.internal.{ Mod, CommandFields }

import scalaz._, Scalaz._, scalaz.effect._

import gem.ctl.free.ctl.UserAndHost
import gem.ctl.free.interpreter.Config


/** A command to be interpreted by gemctl. */
sealed trait Command extends Config

/** Module of constructors of `Command`, and a parser too. */
object Command {

  case class Deploy(
    userAndHost: UserAndHost,
    deployRev:   String,
    standalone:  Boolean,
    verbose:     Boolean
  ) extends Command

  case class Ps(userAndHost: UserAndHost, verbose: Boolean) extends Command
  case class Stop(userAndHost: UserAndHost, verbose: Boolean) extends Command
  case class Log(userAndHost: UserAndHost, verbose: Boolean, count: Int) extends Command

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

  private lazy val host: Parser[String] =
    strOption(
      short('H'), long("host"), metavar("HOST"), value("localhost"),
      help("Docker host, or localhost if unspecified.")
    )

  private lazy val user: Parser[Option[String]] =
    strOption(
      short('u'), long("user"), metavar("USER"), value(null),
      help("Docker user, or current user if unspecified. Passwordless SSH access required.")
    ).map(Option(_))

  private lazy val userAndHost: Parser[UserAndHost] =
    (user |@| host)(UserAndHost)

  private lazy val deploy: Parser[String] =
    strOption(
      short('d'), long("deploy"), metavar("REVISION"), value("HEAD"),
      help("Revision to deploy, HEAD if unspecified.")
    )

  private lazy val standalone: Parser[Boolean] =
    switch(
      short('s'), long("standalone"),
      help("Deploy standalone; do not attempt an upgrade. Cannot be specified with --base")
    )

  private lazy val verbose: Parser[Boolean] =
    switch(
      short('v'), long("verbose"),
      help("Show details about what we're doing under the hood.")
    )

  private lazy val config: Parser[Deploy] =
    (userAndHost |@| deploy |@| standalone |@| verbose)(Deploy.apply)

  private lazy val lines: Parser[Int] = {
    val DefaultLines = 50
    intOption(
      short('n'), metavar("LINES"), value(DefaultLines),
      help(s"Number of lines to show from tail of log (default $DefaultLines)")
    )
  }

  private lazy val configCommand: Mod[CommandFields, Command] =
    command("deploy", info(config.widen[Command] <* helper,
      progDesc("Deploy an application."))
    )

  private lazy val psCommand: Mod[CommandFields, Command] =
    command("ps", info((userAndHost |@| verbose)(Ps).widen[Command] <* helper,
      progDesc("Get the status of a gem deployment."))
    )

  private lazy val stopCommand: Mod[CommandFields, Command] =
    command("stop", info((userAndHost |@| verbose)(Stop).widen[Command] <* helper,
      progDesc("Stop a gem deployment."))
    )

  private lazy val logCommand: Mod[CommandFields, Command] =
    command("log", info((userAndHost |@| verbose |@| lines)(Log).widen[Command] <* helper,
      progDesc("Show the Gem server log."))
    )

  private lazy val mainParser: ParserInfo[Command] =
    info(
      subparser(configCommand, psCommand, stopCommand, logCommand) <*>
      helper, progDesc("Deploy and control gem."))

}
