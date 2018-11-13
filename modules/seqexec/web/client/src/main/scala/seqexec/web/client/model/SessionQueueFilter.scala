// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import monocle.macros.Lenses

sealed trait ObsClass

object ObsClass {
  case object All extends ObsClass
  case object Daytime extends ObsClass
  case object Nighttime extends ObsClass

  implicit val eq: Eq[ObsClass] =
    Eq.fromUniversalEquals
}

/**
  * Model of a filter for the session queue
  */
@Lenses
final case class SessionQueueFilter(obsClass: ObsClass)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SessionQueueFilter {
  implicit val eq: Eq[SessionQueueFilter] =
    Eq.by(_.obsClass)

  val NoFilter: SessionQueueFilter = SessionQueueFilter(ObsClass.All)
}
