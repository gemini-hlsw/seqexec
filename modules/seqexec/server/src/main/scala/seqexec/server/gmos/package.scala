// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.model.enum.NSAction
import seqexec.model.NSSubexposure
import seqexec.engine.Result.PartialVal

package gmos {

  sealed trait NSPartial extends PartialVal {
    def ongoingAction: NSAction
    def sub: NSSubexposure
  }
  object NSPartial {
    def unapply(s: NSPartial): Option[(NSAction, NSSubexposure)] =
      Some((s.ongoingAction, s.sub))

    case class NSStart(sub: NSSubexposure) extends NSPartial {
      override val ongoingAction = NSAction.Start
    }
    case class NSRowsConfigureStart(sub: NSSubexposure) extends NSPartial {
      override val ongoingAction = NSAction.ConfigureRowsStart
    }
    case class NSRowsConfigureComplete(sub: NSSubexposure) extends NSPartial {
      override val ongoingAction = NSAction.ConfigureRowsComplete
    }
    case class NSTCSNodStart(sub: NSSubexposure) extends NSPartial {
      override val ongoingAction = NSAction.NodStart
    }
    case class NSTCSNodComplete(sub: NSSubexposure) extends NSPartial {
      override val ongoingAction = NSAction.NodComplete
    }
    case class NSSubexposureStart(sub: NSSubexposure) extends NSPartial {
      override val ongoingAction = NSAction.StageObserveStart
    }
    case class NSSubexposureEnd(sub: NSSubexposure) extends NSPartial {
      override val ongoingAction = NSAction.StageObserveComplete
    }
    case class NSComplete(sub: NSSubexposure) extends NSPartial {
      override val ongoingAction = NSAction.Done
    }

    case object NSContinue extends InternalPartialVal
    case object NSSubPaused extends InternalPartialVal
    case object NSFinalObs extends InternalPartialVal
  }
}
