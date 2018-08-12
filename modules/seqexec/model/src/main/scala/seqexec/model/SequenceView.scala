// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import monocle.macros.Lenses

import cats._
import cats.implicits._
import gem.Observation

@Lenses
final case class SequenceView (
  id:         Observation.Id,
  metadata:   SequenceMetadata,
  status:     SequenceState,
  steps:      List[Step],
  willStopIn: Option[Int]
)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SequenceView {
  implicit val eq: Eq[SequenceView] =
    Eq.by(x => (x.id, x.metadata, x.status, x.steps, x.willStopIn))
}
