// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.implicits._
import edu.gemini.seqexec.server.gems.{ApdState, BinaryOnOff, ReadyState}

package object gems {

  implicit val readyStateEq: Eq[ReadyState] = Eq.by(_.ordinal)

  implicit val apdStateEq: Eq[ApdState] = Eq.by(_.ordinal)

  implicit val binaryOnOffEq: Eq[BinaryOnOff] = Eq.by(_.ordinal)

}
