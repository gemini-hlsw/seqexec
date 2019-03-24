// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq

package altair {

  sealed trait AOCRFollow extends Product with Serializable

  object AOCRFollow {
    case object Following extends AOCRFollow
    case object Fixed extends AOCRFollow

    implicit val eq: Eq[AOCRFollow] = Eq.fromUniversalEquals
  }
}
