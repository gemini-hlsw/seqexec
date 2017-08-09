// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd

import net.bmjames.opts.types.Parser
import cats._, cats.data._, cats.implicits._
import tuco._, Tuco._

package object command {

  /** Our state is a Service with effect type SessionIO. */
  type GemState = Service[SessionIO]
  object GemState {
    val L: Service.L.type = Service.L // alias the Lens module
  }

  val All: Commands[GemState] =
    Commands[GemState](
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
    parser: Parser[A => SessionIO[A]]
  ): Command[SessionIO, A] =
    shellCommandWithCompleter(name, desc, parser, (_, _) => nil[String].point[SessionIO])

  def shellCommandWithCompleter[A](
    name: String,
    desc: String,
    parser: Parser[A => SessionIO[A]],
    complete: (A, String) => SessionIO[List[String]]
  ): Command[SessionIO, A] =
    Command(name, desc, parser, complete)

}
