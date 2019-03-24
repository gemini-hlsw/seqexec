// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses
import seqexec.model.Notification

/**
  * Utility class to display a generic notification sent by the server
  */
@Lenses
final case class UserNotificationState(visibility: SectionVisibilityState, notification: Option[Notification])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object UserNotificationState {
  val Empty: UserNotificationState = UserNotificationState(SectionClosed, None)

  implicit val eq: Eq[UserNotificationState] =
    Eq.by (x => (x.visibility, x.notification))
}
