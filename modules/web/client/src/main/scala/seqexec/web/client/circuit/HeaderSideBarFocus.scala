// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import monocle.Getter
import monocle.macros.Lenses
import seqexec.model._
import seqexec.web.client.model._
import monocle.Lens

@Lenses
final case class HeaderSideBarFocus(
  status:     ClientStatus,
  conditions: Conditions,
  operator:   Option[Operator]
)

object HeaderSideBarFocus {
  implicit val eq: Eq[HeaderSideBarFocus] =
    Eq.by(x => (x.status, x.conditions, x.operator))

  val headerSideBarG: Getter[SeqexecAppRootModel, HeaderSideBarFocus] =
    Getter[SeqexecAppRootModel, HeaderSideBarFocus] { c =>
      val clientStatus = ClientStatus.clientStatusFocusL.get(c)
      HeaderSideBarFocus(clientStatus, c.sequences.conditions, c.sequences.operator)
    }
}

final case class UserLoginFocus(user: Option[UserDetails], displayNames: Map[String, String]) {
  val displayName: Option[String] = user.flatMap(u => displayNames.get(u.username))
}

object UserLoginFocus {
  implicit val eqUserLoginFocus: Eq[UserLoginFocus] = Eq.by(u => (u.user, u.displayNames))
}

@Lenses
final case class SequencesQueueFocus(
  sequences:   SequencesQueue[SequenceView],
  displayName: Option[String]
)

object SequencesQueueFocus {
  implicit val eqSequencesQueueFocus: Eq[SequencesQueueFocus] =
    Eq.by(u => (u.sequences, u.displayName))

  val sessionQueue: Lens[SequencesQueueFocus, List[SequenceView]] =
    sequences ^|-> SequencesQueue.sessionQueue
}
