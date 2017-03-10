import net.bmjames.opts._
import net.bmjames.opts.types.{ Success, Failure }
import net.bmjames.opts.builder.internal.{ Mod, CommandFields }

import scalaz._, Scalaz._, scalaz.effect._

import ctl.UserAndHost

object opts {

  sealed trait Command {
    def userAndHost: UserAndHost
    def verbose:     Boolean
  }

  case class DeployOpts(
    userAndHost: UserAndHost,
    deployRev:   String,
    standalone:  Boolean,
    verbose:     Boolean
  ) extends Command

  case class PsOpts(userAndHost: UserAndHost, verbose: Boolean) extends Command
  case class StopOpts(userAndHost: UserAndHost, verbose: Boolean) extends Command
  case class LogOpts(userAndHost: UserAndHost, verbose: Boolean, count: Int) extends Command

  val host: Parser[String] =
    strOption(
      short('H'), long("host"), metavar("HOST"), value("localhost"),
      help("Docker host, or localhost if unspecified.")
    )

  val user: Parser[Option[String]] =
    strOption(
      short('u'), long("user"), metavar("USER"), value(null),
      help("Docker user, or current user if unspecified. Passwordless SSH access required.")
    ).map(Option(_))

  val userAndHost: Parser[UserAndHost] =
    (user |@| host)(UserAndHost)

  val deploy: Parser[String] =
    strOption(
      short('d'), long("deploy"), metavar("REVISION"), value("HEAD"),
      help("Revision to deploy, HEAD if unspecified.")
    )

  val standalone: Parser[Boolean] =
    switch(
      short('s'), long("standalone"),
      help("Deploy standalone; do not attempt an upgrade. Cannot be specified with --base")
    )

  val verbose: Parser[Boolean] =
    switch(
      short('v'), long("verbose"),
      help("Show details about what we're doing under the hood.")
    )

  val config: Parser[DeployOpts] =
    (userAndHost |@| deploy |@| standalone |@| verbose)(DeployOpts.apply)

  val lines: Parser[Int] = {
    val DefaultLines = 50
    intOption(
      short('n'), metavar("LINES"), value(DefaultLines),
      help(s"Number of lines to show from tail of log (default $DefaultLines)")
    )
  }

  // Commands

  val configCommand: Mod[CommandFields, Command] =
    command("deploy", info(config.widen[Command] <* helper,
      progDesc("Deploy an application."))
    )

  val psCommand: Mod[CommandFields, Command] =
    command("ps", info((userAndHost |@| verbose)(PsOpts).widen[Command] <* helper,
      progDesc("Get the status of a gem deployment."))
    )

  val stopCommand: Mod[CommandFields, Command] =
    command("stop", info((userAndHost |@| verbose)(StopOpts).widen[Command] <* helper,
      progDesc("Stop a gem deployment."))
    )

  val logCommand: Mod[CommandFields, Command] =
    command("log", info((userAndHost |@| verbose |@| lines)(LogOpts).widen[Command] <* helper,
      progDesc("Show the Gem server log."))
    )

  // Main

  val mainParser: ParserInfo[Command] =
    info(
      subparser(configCommand, psCommand, stopCommand, logCommand) <*>
      helper, progDesc("Deploy and control gem."))

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

}
