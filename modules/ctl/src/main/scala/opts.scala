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

  val sample: Parser[Config] =
    (
      strOption(short('H'), long("host"),    metavar("[USER@]HOST"), value("localhost"), help("Docker user and host, localhost if unspecified. Passwordless SSH access required."))    |@|
      strOption(short('d'), long("deploy"),  metavar("REVISION"),    value("HEAD"),      help("Revision to deploy, HEAD if unspecified."))    |@|
      strOption(short('b'), long("base"),    metavar("REVISION"),                         help("Base revision to upgrade from. Must be an ancestor of the base revision. Defaults to the most recent version tagged 'deploy-xxx'. Cannot be specified with --standalone."), value(null)).map(Option(_)) |@|
      switch(   short('s'), long("standalone"),                                           help("Deploy standalone; do not attempt an upgrade. Cannot be specified with --base"))
    )(Config.apply)

  val opts = info(
    subparser(command("deploy", info(sample <* helper, progDesc("Deploy an application. This is a very long description indeed. Will it wrap? We must test it out I guess.")))) <* helper,
    progDesc("Deploy and control gem."))

  def parse[A](progName: String, args: List[String]): IO[Option[Config]] =
    execParserPure(prefs(idm[PrefsMod]), opts, args) match {
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
