package gem
package telnetd

import gem.enum.ProgramRole
import net.bmjames.opts. { Parser => _, _}
import net.bmjames.opts.types._
import tuco.imports._
import scalaz._, Scalaz._

object GemCommands {
  import Session.{ L => SL }
  import Data.{ L => DL }

  val whoami = Command(
    "whoami", "Answer existential questions.",
    Parser.pure((u: User[ProgramRole]) => HC.writeLn(s"${u.id}").as(u))
  ).zoom(SL.data[Data] >=> DL.user)

  val all = Commands[Data](
    whoami
  )

}
