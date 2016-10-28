package gem
package telnetd
package command

import gem.enum.ProgramRole
import net.bmjames.opts.types._
import tuco._, Tuco._

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
