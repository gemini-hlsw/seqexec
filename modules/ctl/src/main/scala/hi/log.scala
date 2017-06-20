// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import gem.ctl.free.ctl.{ CtlIO, text }
import gem.ctl.low.io.Output
import gem.ctl.low.docker.docker
import gem.ctl.hi.common.getRunningGemContainer

import scalaz._, Scalaz._

/** Constructors for `CtlIO` operations related to the `log` command. */
object log {

  def showLog(lines: Int): CtlIO[Unit] =
    getRunningGemContainer.flatMap { c =>
      docker("logs", "--tail", lines.toString, c.hash).require {
        case Output(0, ss) => ss
      } flatMap { ss =>
        ss.traverse_(text)
      }
    }
}
