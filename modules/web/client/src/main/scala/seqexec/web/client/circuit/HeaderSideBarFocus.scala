// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import monocle.Getter
import monocle.macros.Lenses
// import seqexec.model.Observation
// import seqexec.model.QueueId
import seqexec.model._
// import seqexec.model.enum.Instrument
import seqexec.web.client.model._

@Lenses
final case class HeaderSideBarFocus(
  status:      ClientStatus,
  conditions:  Conditions,
  operator:    Option[Operator],
  displayName: Option[String]
)

object HeaderSideBarFocus {
  implicit val eq: Eq[HeaderSideBarFocus] =
    Eq.by(x => (x.status, x.conditions, x.operator)) //, x.observer))

  val headerSideBarG: Getter[SeqexecAppRootModel, HeaderSideBarFocus] =
    Getter[SeqexecAppRootModel, HeaderSideBarFocus] { c =>
      val clientStatus = ClientStatus(c.uiModel.user, c.ws)
      val displayName  = c.uiModel.user.flatMap(u => c.uiModel.displayNames.get(u.username))
      HeaderSideBarFocus(clientStatus, c.sequences.conditions, c.sequences.operator, displayName)
    }
}
