package gem
package telnetd

import net.bmjames.opts.types.Parser
import scalaz._, Scalaz._
import tuco._, Tuco._

package object command {

  /** Our state is a Service with effect type SessionIO. */
  type GemState = Service[SessionIO]
  object GemState {
    val L = Service.L // alias the Lens module
  }

  val All = Commands[GemState](
    ls.command,
    whoami.command,
    passwd.command
  )

  /** Our commands are in SessionIO, and pass a Session[GemState] around. */
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
