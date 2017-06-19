// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd
package command

import net.bmjames.opts.types._
import tuco._, Tuco._

/** A command for changing passwords. */
object passwd {

  val command: GemCommand =
    shellCommand[GemState](
      "passwd", "Change password.",
      Parser.pure { s =>
        for {
          o <- readLn("Old password: ", mask = Some('*'))
          n <- readLn("New password: ", mask = Some('*'))
          b <- s.changePassword(o, n)
          _ <- if (b) writeLn("Password changed.")
               else   writeLn("Incorrect old password and/or invalid new password.")
        } yield s
      }
    ).zoom(Session.L.data[GemState])

}
