package gem
package telnetd

import net.bmjames.opts.types.Parser
import scalaz._, Scalaz._
import tuco._, Tuco._

package object command {

  type GemState = Service[SessionIO]
  object GemState {
    val L = Service.L
  }

  val All = Commands[GemState](
    ls.command,
    whoami.command,
    passwd.command
  )

  type GemCommand = Command[SessionIO, Session[GemState]]

  // Our commnds are always in SessionIO.
  def shellCommand[A](
    name: String,
    desc: String,
    parser: Parser[A => SessionIO[A]],
    complete: (A, String) => SessionIO[List[String]] = (a: A, s: String) => nil[String].point[SessionIO]
  ): Command[SessionIO, A] =
    Command(name, desc, parser, complete)

}
