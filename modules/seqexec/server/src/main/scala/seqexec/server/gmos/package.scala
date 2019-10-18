// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.model.enum.NSAction
import seqexec.model.NSSubexposure
import seqexec.engine.Result.PartialVal

package object gmos {
  type GmosSouthController[F[_]] = GmosController[F, GmosController.SouthTypes]

  type GmosNorthController[F[_]] = GmosController[F, GmosController.NorthTypes]
}

package gmos {

  import gem.util.Enumerated

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

  sealed trait NSObserveCommand extends Product with Serializable

  object NSObserveCommand {
    case object StopGracefully extends NSObserveCommand
    case object StopImmediately extends NSObserveCommand
    case object AbortGracefully extends NSObserveCommand
    case object AbortImmediately extends NSObserveCommand
    case object PauseGracefully extends NSObserveCommand
    case object PauseImmediately extends NSObserveCommand

    implicit val nsObserveCommandEnum: Enumerated[NSObserveCommand] =
      Enumerated.of(
        StopGracefully, StopImmediately,AbortGracefully, AbortImmediately, PauseGracefully, PauseImmediately
      )
  }

}
