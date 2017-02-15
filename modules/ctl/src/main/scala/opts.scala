import net.bmjames.opts._

import scalaz._, Scalaz._

object opts {

  case class Config(
    deployRev:  String,
    baseRev:    Option[String],
    standalone: Boolean
  )

  val sample: Parser[Config] =
    (
      strOption(short('d'), long("deploy"),     metavar("«deploy-rev»"),          value("HEAD"), help("Revision to deploy, HEAD if unspecified."))    |@|
      strOption(short('b'), long("base"),       metavar("«base-rev»"),                  help("Base revision to upgrade from. Must be an ancestor of «base-rev». Defaults to the most recent version tagged 'deploy-xxx'. Cannot be specified with --standalone."), value(null)).map(Option(_)) |@|
      switch(   short('s'), long("standalone"),                                         help("Deploy standalone; do not attempt an upgrade. Cannot be specified with --base"))
    )(Config.apply)

  val opts = info(subparser(command("deploy", info(sample <* helper, progDesc("Deploy an application.")))) <* helper,
    progDesc("Gem control thingy. Try -h for help."))

  def parse[A](args: List[String])(f: Config => A): A =
    f(execParser(args.toArray, "gemctl", opts))

}
