package gem
package telnetd
package command

import gem.dao.ProgramDao
import net.bmjames.opts.{ strArgument, help, metavar }
import net.bmjames.opts.types.Parser
import tuco._, Tuco._
import scalaz._, Scalaz._

object cd {

  val command: GemCommand = {

    val pid: Parser[Program.Id] =
      strArgument(
        help("Program id."),
        metavar("<program id>")
      ).map(Program.Id.parse)

    shellCommand[GemState](
      "cd", "Move into the specified program.",
      pid.map { id => d =>
        // TODO: something
        d.point[SessionIO]
      },
      (d: GemState, s: String) => {
        (d.queryProgramsByName(s"${s.toLowerCase}%", Int.MaxValue))
          .map(_.map(_.id.toString))
      }
    ).zoom(Session.L.data[GemState])

  }

}
