// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.message

import cats.Eq

/**
  * Common code for all Message components
  */
trait Message {
  sealed trait Style

  object Style {
    case object NotDefined extends Style
    case object Warning extends Style
    case object Info extends Style
    case object Positive extends Style
    case object Success extends Style
    case object Negative extends Style
    case object Error extends Style

    implicit val equal: Eq[Style] = Eq.fromUniversalEquals
  }
}
