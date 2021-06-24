// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import edu.gemini.seqexec.server.gems.ApdState
import edu.gemini.seqexec.server.gems.BinaryOnOff
import edu.gemini.seqexec.server.gems.ReadyState

package object gems {

  implicit val readyStateEq: Eq[ReadyState] = Eq.by(_.ordinal)

  implicit val apdStateEq: Eq[ApdState] = Eq.by(_.ordinal)

  implicit val binaryOnOffEq: Eq[BinaryOnOff] = Eq.by(_.ordinal)

}
