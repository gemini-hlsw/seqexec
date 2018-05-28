// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.implicits._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}

package object tcs {
  implicit val ooEq: Eq[BinaryOnOff] =
    Eq[Int].contramap(_.ordinal())
  implicit val ynEq: Eq[BinaryYesNo] =
    Eq[Int].contramap(_.ordinal())
}
