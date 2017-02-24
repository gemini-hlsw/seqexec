import net.bmjames.opts._

import scalaz._, Scalaz._

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

  val opts = info(subparser(command("deploy", info(sample <* helper, progDesc("Deploy an application. This is a very long description indeed. Will it wrap? We must test it out I guess.")))) <* helper,
    progDesc("Gem control thingy. Try -h for help."))

  def parse[A](args: List[String])(f: Config => A): A =
    f(execParser(args.toArray, "gemctl", opts))

}
