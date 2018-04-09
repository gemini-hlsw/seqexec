// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import cats.Eq
import cats.implicits._

package object tcs {
  implicit val ooEq: Eq[BinaryOnOff] =
    Eq[Int].contramap(_.ordinal())
  implicit val ynEq: Eq[BinaryYesNo] =
    Eq[Int].contramap(_.ordinal())
}
