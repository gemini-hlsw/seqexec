// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.implicits._
import edu.gemini.seqexec.server.nifs.DhsConnected

package object nifs {
  implicit val eqDhsConnect: Eq[DhsConnected] = Eq.by(_.ordinal)
}
