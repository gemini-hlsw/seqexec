// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import monocle.macros.Lenses
import seqexec.common.FixedLengthBuffer
import seqexec.model.events._

/**
  * Keeps a list of log entries for display
  */
@Lenses
final case class GlobalLog(log:     FixedLengthBuffer[ServerLogMessage],
                           display: SectionVisibilityState)

object GlobalLog {
  implicit val eq: Eq[GlobalLog] = Eq.by(x => (x.log, x.display))
}
