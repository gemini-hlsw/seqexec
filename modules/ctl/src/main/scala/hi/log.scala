package gem.ctl.hi

import gem.ctl.free.ctl.{ log => ctlLog, CtlIO, Shell }
import gem.ctl.low.io.Output
import gem.ctl.low.docker.docker
import gem.ctl.hi.common.getRunningGemContainer

import scalaz._, Scalaz._

object log {

  def showLog(lines: Int): CtlIO[Unit] =
    getRunningGemContainer.flatMap { c =>
      docker("logs", "--tail", lines.toString, c.hash).require {
        case Output(0, ss) => ss
      } flatMap { ss =>
        ss.traverse_(ctlLog(Shell, _))
      }
    }
}
