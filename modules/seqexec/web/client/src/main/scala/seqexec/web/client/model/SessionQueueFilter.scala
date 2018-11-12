// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses

/**
  * Model of a filter for the session queue
  */
@Lenses
final case class SessionQueueFilter(classExcluded: List[String])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SessionQueueFilter {
  implicit val eq: Eq[SessionQueueFilter] =
    Eq.by(_.classExcluded)

  val NoFilter: SessionQueueFilter = SessionQueueFilter(List.empty)
}
