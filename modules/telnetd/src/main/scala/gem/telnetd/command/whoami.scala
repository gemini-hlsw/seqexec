// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd
package command

import cats.Applicative
import gem.enum.ProgramRole
import com.monovore.decline.{ Command => _, _ }
import tuco._, Tuco._, tuco.shell._

/** A command to show information about the current user. */
object whoami {

  val command: GemCommand =
    shellCommand[User[ProgramRole]](
      "whoami", "Show information about the current user.",
      Applicative[Opts].pure { u =>
        for {
          _ <- writeLn(s"username: ${u.id}")
          _ <- writeLn(s"   first: ${u.firstName}")
          _ <- writeLn(s"    last: ${u.lastName}")
          _ <- writeLn(s"   email: ${u.email}")
          _ <- writeLn(s"   flags: ${if (u.isStaff) "staff" else "<none>"}")
        } yield u
      }
    ).zoom(Session.data[GemState] ^|-> Service.user[SessionIO])

}
