// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.engine.Result.PartialVal
import seqexec.model.dhs.ImageFileId
import squants.Time

// Marker trait for partials that won't result on a client message
trait InternalPartialVal extends PartialVal

final case class FileIdAllocated(fileId: ImageFileId) extends PartialVal
final case class RemainingTime(self: Time) extends AnyVal
final case class Progress(total: Time, remaining: RemainingTime) extends PartialVal {
  val progress: Time = total - remaining.self
}
