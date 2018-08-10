// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.Show
import cats.implicits._
import cats.data.NonEmptyList
import gem.enum.Site
import seqexec.model.enum.{ ActionStatus, Instrument, Resource }
import seqexec.model.{ StepState, SequenceState, Step, StandardStep }
import seqexec.web.client.model.RunningStep
import seqexec.model.SequenceView

/**
  * Contains useful operations for the seqexec model
  */
object ModelOps {

  implicit val sequenceStateShow: Show[SequenceState] =
    Show.show[SequenceState] {
      case SequenceState.Completed        => "Complete"
      case SequenceState.Running(true, _) => "Pausing..."
      case SequenceState.Running(_, _)    => "Running"
      case SequenceState.Idle             => "Idle"
      case SequenceState.Stopped          => "Stopped"
      case SequenceState.Failed(_)        => s"Error at step "
    }

  implicit val stepShow: Show[Step] = Show.show[Step] { s =>
    s.status match {
      case StepState.Pending                      => "Pending"
      case StepState.Completed                    => "Done"
      case StepState.Skipped                      => "Skipped"
      case StepState.Failed(msg)                  => msg
      case StepState.Running if s.isObserving     => "Observing..."
      case StepState.Running if s.isObservePaused => "Exposure paused"
      case StepState.Running if s.isConfiguring   => "Configuring..."
      case StepState.Running                      => "Running..."
      case StepState.Paused                       => "Paused"
    }
  }

  implicit val resourceShow: Show[Resource] = Show.show[Resource] {
    case Resource.TCS    => "TCS"
    case Resource.Gcal   => "GCAL"
    case Resource.Gems   => "GeMS"
    case Resource.Altair => "Altair"
    case Resource.P1     => "P1"
    case Resource.OI     => "OI"
    case i: Instrument   => i.show
  }

  implicit class SequenceViewOps(val s: SequenceView) extends AnyVal {

    def progress: RunningStep =
      RunningStep(s.steps.count(_.isFinished), s.steps.length)

    // Returns where on the sequence the execution is at
    def runningStep: Option[RunningStep] = s.status match {
      case SequenceState.Running(_, _) => Some(progress)
      case SequenceState.Failed(_)     => Some(progress)
      case _                           => None
    }

    def allStepsDone: Boolean = s.steps.forall(_.status === StepState.Completed)

    def flipSkipMarkAtStep(step: Step): SequenceView =
      s.copy(steps = s.steps.collect {
        case st: StandardStep if st == step => st.copy(skip = !st.skip)
        case st                             => st
      })

    def flipBreakpointAtStep(step: Step): SequenceView =
      s.copy(steps = s.steps.collect {
        case st: StandardStep if st == step =>
          st.copy(breakpoint = !st.breakpoint)
        case st => st
      })

    def nextStepToRun: Option[Int] =
      s.steps match {
        case x if x.forall(s => s.status === StepState.Pending && !s.skip) =>
          Some(0) // No steps have been executed, start at 0
        case x if x.forall(_.isFinished) => None // All steps have been executed
        case x if x.exists(_.hasError) =>
          Option(x.indexWhere((s: Step) => s.hasError)).filter(_ =!= -1)
        case x if x.exists(s => s.status === StepState.Paused && !s.skip) =>
          Option(x.indexWhere((s: Step) => s.status === StepState.Paused))
            .filter(_ =!= -1)
        case x =>
          Option(x.indexWhere((s: Step) => !s.isFinished && !s.skip))
            .filter(_ =!= -1)
      }

    def isPartiallyExecuted: Boolean = s.steps.exists(_.isFinished)

    def showAsRunning(i: Int): SequenceView =
      s.copy(steps = s.steps.collect {
        case s: StandardStep if s.id === i =>
          s.copy(
            status = StepState.Running,
            configStatus = List((Resource.TCS, ActionStatus.Pending),
                                (Resource.Gcal, ActionStatus.Running),
                                (Instrument.GPI, ActionStatus.Completed))
          )
        case s => s
      })
  }

  implicit class SiteOps(val s: Site) extends AnyVal {

    def instruments: NonEmptyList[Instrument] = s match {
      case Site.GN => Instrument.gnInstruments
      case Site.GS => Instrument.gsInstruments
    }
  }

  sealed trait InstrumentProperties

  object InstrumentProperties {
    case object Disperser     extends InstrumentProperties
    case object Offsets       extends InstrumentProperties
    case object FPU           extends InstrumentProperties
    case object ObservingMode extends InstrumentProperties
  }

  implicit class InstrumentOps(val i: Instrument) extends AnyVal {

    def displayItems: Set[InstrumentProperties] = i match {
      case Instrument.F2 =>
        Set(InstrumentProperties.Offsets, InstrumentProperties.FPU)
      case Instrument.GmosS =>
        Set(InstrumentProperties.Offsets,
            InstrumentProperties.Disperser,
            InstrumentProperties.FPU)
      case Instrument.GmosN =>
        Set(InstrumentProperties.Offsets,
            InstrumentProperties.Disperser,
            InstrumentProperties.FPU)
      case Instrument.GNIRS =>
        Set(InstrumentProperties.Offsets,
            InstrumentProperties.Disperser,
            InstrumentProperties.FPU)
      case Instrument.GPI =>
        Set(InstrumentProperties.ObservingMode, InstrumentProperties.Disperser)
      case Instrument.GHOST => Set.empty
      case _                => Set(InstrumentProperties.Offsets, InstrumentProperties.FPU)
    }
  }

}
