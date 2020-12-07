// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import edu.gemini.seqexec.server.gsaoi.DhsConnected

package gsaoi {
  trait GsaoiFullHandler[F[_]] extends GsaoiController[F] with GsaoiGuider[F]
}

package object gsaoi {
  implicit val dhsConnectedEq: Eq[DhsConnected] = Eq.by(_.ordinal)
}
