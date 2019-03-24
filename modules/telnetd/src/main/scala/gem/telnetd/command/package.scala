// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd

import cats.implicits._
import com.monovore.decline.{ Command => _, _ }
import tuco._, Tuco._, tuco.shell._

package object command {

  /** Our state is a Service with effect type SessionIO. */
  type GemState = Service[SessionIO]

  val All: Commands[GemState] =
    Commands[GemState](
      eph.exportCommand,
      eph.reportCommand,
      eph.updateCommand,
      fetch.obsCommand,
      fetch.progCommand,
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
    parser: Opts[A => SessionIO[A]]
  ): Command[SessionIO, A] =
    shellCommandWithCompleter(name, desc, parser, (_, _) => List.empty[String].pure[SessionIO])

  def shellCommandWithCompleter[A](
    name: String,
    desc: String,
    parser: Opts[A => SessionIO[A]],
    complete: (A, String) => SessionIO[List[String]]
  ): Command[SessionIO, A] =
    Command(name, desc, parser, complete)

}
