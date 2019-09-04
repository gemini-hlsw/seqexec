// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.implicits._
import cats.Eq
import edu.gemini.seqexec.server.gcal.BinaryOnOff

package object gcal {
  implicit val binaryOnOffEq: Eq[BinaryOnOff] = Eq.by(_.ordinal())
}
