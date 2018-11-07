// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import squants.Time
import seqexec.model.dhs.ImageFileId

final case class ObservationProgress(fileId:    ImageFileId,
                                     total:     Time,
                                     remaining: Time)

object ObservationProgress {

  implicit val equal: Eq[ObservationProgress] =
    Eq.by(x => (x.fileId, x.total, x.remaining))

}
