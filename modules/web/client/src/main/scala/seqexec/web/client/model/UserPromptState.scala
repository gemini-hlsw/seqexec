// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import monocle.macros.Lenses
import seqexec.model.UserPrompt

/**
 * Utility class to display a generic notification sent by the server
 */
@Lenses
final case class UserPromptState(notification: Option[UserPrompt])

object UserPromptState {
  val Empty: UserPromptState = UserPromptState(None)

  implicit val eq: Eq[UserPromptState] =
    Eq.by(_.notification)
}
