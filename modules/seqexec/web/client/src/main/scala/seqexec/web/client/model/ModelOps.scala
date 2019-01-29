// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Show
import cats.implicits._
import cats.data.NonEmptyList
import gem.enum.Site
import gem.enum.GpiDisperser
import gem.enum.GpiObservingMode
import gem.enum.GpiFilter
import seqexec.model.enum.ActionStatus
import seqexec.model.enum.Instrument
import seqexec.model.enum.Resource
import seqexec.model.enum.FPUMode
import seqexec.model.StepState
import seqexec.model.SequenceState
import seqexec.model.Step
import seqexec.model.StandardStep
import seqexec.model.SequenceView
import seqexec.model.enumerations
import seqexec.web.client.model.lenses._
import seqexec.web.client.model.Formatting._

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

    def allStepsDone: Boolean = s.steps.forall(_.status === StepState.Completed)

    def flipSkipMarkAtStep(step: Step): SequenceView =
      s.copy(steps = s.steps.collect {
        case st: StandardStep if st.id === step.id => st.copy(skip = !st.skip)
        case st                                    => st
      })

    def flipBreakpointAtStep(step: Step): SequenceView =
      s.copy(steps = s.steps.collect {
        case st: StandardStep if st.id === step.id =>
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
                                (Instrument.Gpi, ActionStatus.Completed))
          )
        case s => s
      })
  }

  private val gpiObsMode = GpiObservingMode.all.map(x => x.shortName -> x).toMap

  private val gpiFiltersMap: Map[String, GpiFilter] =
    GpiFilter.all.map(x => (x.shortName, x)).toMap

  val gpiDispersers: Map[String, String] =
    GpiDisperser.all.map(x => x.shortName -> x.longName).toMap

  implicit class StepOps(val s: Step) extends AnyVal {
    def fpuNameMapper(i: Instrument): String => Option[String] = i match {
      case Instrument.GmosS => enumerations.fpu.GmosSFPU.get
      case Instrument.GmosN => enumerations.fpu.GmosNFPU.get
      case Instrument.F2    => enumerations.fpu.Flamingos2.get
      case _ =>
        _ =>
          none
    }

    def exposureTime: Option[Double] = observeExposureTimeO.getOption(s)
    def exposureTimeS(i: Instrument): Option[String] =
      exposureTime.map(formatExposureTime(i))
    def coAdds: Option[Int] = observeCoaddsO.getOption(s)
    def fpu(i: Instrument): Option[String] =
      for {
        mode <- instrumentFPUModeO
          .getOption(s)
          .orElse(FPUMode.BuiltIn.some) // If the instrument has no fpu mode default to built in
        fpuL = if (mode === FPUMode.BuiltIn) instrumentFPUO
        else instrumentFPUCustomMaskO
        fpu <- fpuL.getOption(s)
      } yield fpuNameMapper(i)(fpu).getOrElse(fpu)

    def fpuOrMask(i: Instrument): Option[String] =
      fpu(i)
        .orElse(instrumentSlitWidthO.getOption(s))
        .orElse(instrumentMaskO.getOption(s))

    private def gpiFilter: Step => Option[String] = s => {
      // Read the filter, if not found deduce it from the obs mode
      val f: Option[GpiFilter] =
        instrumentFilterO.getOption(s).flatMap(gpiFiltersMap.get).orElse {
          for {
            m <- instrumentObservingModeO.getOption(s)
            o <- gpiObsMode.get(m)
            f <- o.filter
          } yield f
        }
      f.map(_.longName)
    }

    def filter(i: Instrument): Option[String] = i match {
      case Instrument.GmosS =>
        instrumentFilterO
          .getOption(s)
          .flatMap(enumerations.filter.GmosSFilter.get)
      case Instrument.GmosN =>
        instrumentFilterO
          .getOption(s)
          .flatMap(enumerations.filter.GmosNFilter.get)
      case Instrument.F2 =>
        instrumentFilterO
          .getOption(s)
          .flatMap(enumerations.filter.F2Filter.get)
      case Instrument.Niri =>
        instrumentFilterO
          .getOption(s)
          .flatMap(enumerations.filter.Niri.get)
      case Instrument.Gnirs =>
        instrumentFilterO
          .getOption(s)
          .map(_.sentenceCase)
      case Instrument.Nifs =>
        instrumentFilterO
          .getOption(s)
          .map(_.sentenceCase)
      case Instrument.Gpi => gpiFilter(s)
      case _              => None
    }

    private def disperserNameMapper(i: Instrument): Map[String, String] =
      i match {
        case Instrument.GmosS => enumerations.disperser.GmosSDisperser
        case Instrument.GmosN => enumerations.disperser.GmosNDisperser
        case Instrument.Gpi   => gpiDispersers
        case _                => Map.empty
      }

    def disperser(i: Instrument): Option[String] = {
      val disperser = for {
        disperser <- instrumentDisperserO.getOption(s)
      } yield disperserNameMapper(i).getOrElse(disperser, disperser)
      val centralWavelength = instrumentDisperserLambdaO.getOption(s)

      // Format
      (disperser, centralWavelength) match {
        case (Some(d), Some(w)) => f"$d @ $w%.0f nm".some
        case (Some(d), None)    => d.some
        case _                  => none
      }

    def canRunFrom: Boolean = s.status match {
      case StepState.Pending | StepState.Failed(_) => true
      case _                                       => false
    
    }
  }

  implicit class SiteOps(val s: Site) extends AnyVal {

    def instruments: NonEmptyList[Instrument] = s match {
      case Site.GN => Instrument.gnInstruments
      case Site.GS => Instrument.gsInstruments
    }
  }

  implicit class ExtraStringOps(val s: String) extends AnyVal {
    def sentenceCase: String =
      (s.toList match {
        case Nil       => Nil
        case x :: rest => x.toUpper :: rest.map(_.toLower)
      }).mkString
  }

  sealed trait InstrumentProperties

  object InstrumentProperties {
    case object Exposure      extends InstrumentProperties
    case object Filter        extends InstrumentProperties
    case object Disperser     extends InstrumentProperties
    case object Offsets       extends InstrumentProperties
    case object FPU           extends InstrumentProperties
    case object ObservingMode extends InstrumentProperties
    case object Camera        extends InstrumentProperties
    case object Decker        extends InstrumentProperties
    case object ImagingMirror extends InstrumentProperties
    case object ReadMode      extends InstrumentProperties
  }

  implicit class InstrumentOps(val i: Instrument) extends AnyVal {

    def displayItems: Set[InstrumentProperties] = i match {
      case Instrument.F2 =>
        Set(InstrumentProperties.Exposure,
            InstrumentProperties.Filter,
            InstrumentProperties.Offsets,
            InstrumentProperties.FPU)
      case Instrument.Nifs =>
        Set(
          InstrumentProperties.Exposure,
          InstrumentProperties.Filter,
          InstrumentProperties.Offsets,
          InstrumentProperties.Disperser,
          InstrumentProperties.FPU,
          InstrumentProperties.ImagingMirror
        )
      case Instrument.GmosS =>
        Set(InstrumentProperties.Exposure,
            InstrumentProperties.Filter,
            InstrumentProperties.Offsets,
            InstrumentProperties.Disperser,
            InstrumentProperties.FPU)
      case Instrument.GmosN =>
        Set(InstrumentProperties.Exposure,
            InstrumentProperties.Filter,
            InstrumentProperties.Offsets,
            InstrumentProperties.Disperser,
            InstrumentProperties.FPU)
      case Instrument.Gnirs =>
        Set(
          InstrumentProperties.Exposure,
          InstrumentProperties.Filter,
          InstrumentProperties.Offsets,
          InstrumentProperties.Disperser,
          InstrumentProperties.Decker,
          InstrumentProperties.FPU
        )
      case Instrument.Gpi =>
        Set(InstrumentProperties.Exposure,
            InstrumentProperties.Filter,
            InstrumentProperties.ObservingMode,
            InstrumentProperties.Disperser)
      case Instrument.Niri =>
        Set(InstrumentProperties.Exposure,
            InstrumentProperties.Offsets,
            InstrumentProperties.Filter,
            InstrumentProperties.Camera)
      case Instrument.Gsaoi =>
        Set(InstrumentProperties.Exposure,
            InstrumentProperties.Offsets,
            InstrumentProperties.Filter,
            InstrumentProperties.ReadMode)
      case Instrument.Ghost => Set.empty
      case _                =>
        Set(InstrumentProperties.Exposure,
            InstrumentProperties.Filter,
            InstrumentProperties.Offsets,
            InstrumentProperties.FPU)
    }
  }

}
