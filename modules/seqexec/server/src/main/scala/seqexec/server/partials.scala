// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.engine.Result.PartialVal
import seqexec.model.dhs.ImageFileId
import seqexec.model.{NSSubexposure, ObserveStage}
import squants.Time

// Marker trait for partials that won't result on a client message
trait InternalPartialVal extends PartialVal

final case class FileIdAllocated(fileId: ImageFileId) extends PartialVal
final case class RemainingTime(self: Time) extends AnyVal

sealed trait Progress extends PartialVal with Product with Serializable {
  val total: Time
  val remaining: RemainingTime
  def progress: Time
  val stage: ObserveStage
}

object Progress {
  implicit class ProgressOps(val a: Progress) extends AnyVal {
    def toNSProgress(sub: NSSubexposure): NSProgress =
      NSProgress.fromObsProgress(a, sub)
  }
}

final case class ObsProgress(total: Time, remaining: RemainingTime, stage: ObserveStage) extends Progress {
  val progress: Time = total - remaining.self
}

final case class NSProgress(total: Time, remaining: RemainingTime, stage: ObserveStage, sub: NSSubexposure) extends Progress {
  val progress: Time = total - remaining.self
}

object NSProgress {
  def fromObsProgress(progress: Progress, sub: NSSubexposure): NSProgress =
    NSProgress(progress.total, progress.remaining, progress.stage, sub)
}
