// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._

final case class RunningStep(last: Int, total: Int)

object RunningStep {
  implicit val show: Show[RunningStep] =
    Show.show(u => s"${u.last + 1}/${u.total}")

  implicit val eq: Eq[RunningStep] =
    Eq.by(x => (x.last, x.total))
}
