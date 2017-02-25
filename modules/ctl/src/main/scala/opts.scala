import net.bmjames.opts._
import net.bmjames.opts.types.{ Success, Failure }

import scalaz.Scalaz._, scalaz.effect._

object opts {

  case class Config(
    userAndHost: String,
    deployRev:   String,
    baseRev:     Option[String],
    standalone:  Boolean
  )

  val host: Parser[String] =
    strOption(
      short('H'), long("host"), metavar("[USER@]HOST"), value("localhost"),
      help("Docker user and host, localhost if unspecified. Passwordless SSH access required.")
    )

  val deploy: Parser[String] =
    strOption(
      short('d'), long("deploy"), metavar("REVISION"), value("HEAD"),
      help("Revision to deploy, HEAD if unspecified.")
    )

  val base: Parser[Option[String]] =
    strOption(
      short('b'), long("base"), metavar("REVISION"), value(null),
      help("Base revision to upgrade from. Must be an ancestor of the base revision. Defaults to the most recent version tagged 'deploy-xxx'. Cannot be specified with --standalone.")
    ).map(Option(_))

  val standalone: Parser[Boolean] =
    switch(
      short('s'), long("standalone"),
      help("Deploy standalone; do not attempt an upgrade. Cannot be specified with --base")
    )

  val config: Parser[Config] =
    (host |@| deploy |@| base |@| standalone)(Config.apply)

  val configCommand: Parser[Config] =
    subparser(command("deploy", info(config <* helper,
      progDesc("Deploy an application. This is a very long description indeed. Will it wrap? We must test it out I guess."))
    ))

  val mainParser: ParserInfo[Config] =
    info(configCommand <* helper, progDesc("Deploy and control gem."))

  def parse[A](progName: String, args: List[String]): IO[Option[Config]] =
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
