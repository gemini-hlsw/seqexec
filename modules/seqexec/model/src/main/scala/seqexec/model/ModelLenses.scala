// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import seqexec.model.enum._
import seqexec.model.events._

import monocle.{Lens, Optional, Prism, Traversal}
import monocle.macros.{GenLens, GenPrism}
import monocle.function.At.{at, atMap}
import monocle.function.FilterIndex.{filterIndex}
import monocle.unsafe.MapTraversal._
import monocle.std.option.some
import monocle.Iso

import cats._
import cats.implicits._
import mouse.all._

trait ModelLenses {
    // Some useful Monocle lenses
  val obsNameL: Lens[SequenceView, String] = GenLens[SequenceView](_.metadata.name)
  // From step to standard step
  val standardStepP: Prism[Step, StandardStep] = GenPrism[Step, StandardStep]
  val eachStepT: Traversal[List[Step], Step] = Traversal.fromTraverse[List, Step]
  val obsStepsL: Lens[SequenceView, List[Step]] = GenLens[SequenceView](_.steps)
  val eachViewT: Traversal[List[SequenceView], SequenceView] = Traversal.fromTraverse[List, SequenceView]
  val sequencesQueueL: Lens[SequencesQueue[SequenceView], List[SequenceView]] = GenLens[SequencesQueue[SequenceView]](_.queue)
  // from standard step to config
  val stepConfigL: Lens[StandardStep, StepConfig] = GenLens[StandardStep](_.config)
  // Prism to focus on only the SeqexecEvents that have a queue
  val sequenceEventsP: Prism[SeqexecEvent, SeqexecModelUpdate] = GenPrism[SeqexecEvent, SeqexecModelUpdate]
  // Required for type correctness
  val stepConfigRoot: Iso[Map[SystemName, Parameters], Map[SystemName, Parameters]] = Iso.id[Map[SystemName, Parameters]]
  val parametersRoot: Iso[Map[ParamName, ParamValue], Map[ParamName, ParamValue]] = Iso.id[Map[ParamName, ParamValue]]

  // Focus on a param value
  def paramValueL(param: ParamName): Lens[Parameters, Option[String]] =
    parametersRoot ^|-> // map of parameters
    at(param)           // parameter containing the name

  // Focus on params with a prefix
  def paramValuesWithPrefixT(param: ParamName): Traversal[Parameters, String] =
    parametersRoot                                     ^|->> // map of parameters
    filterIndex { n: ParamName => n.startsWith(param)}       // parameter containing the name

  // Possible set of observe parameters
  def systemConfigL(system: SystemName): Lens[StepConfig, Option[Parameters]] =
    stepConfigRoot ^|-> // map of systems
    at(system)          // subsystem name

  // Param name of a StepConfig
  def configParamValueO(system: SystemName, param: String): Optional[StepConfig, String] =
    systemConfigL(system)                ^<-? // observe parameters
    some                                 ^|-> // focus on the option
    paramValueL(system.withParam(param)) ^<-? // find the target name
    some                                      // focus on the option

  // Focus on the sequence view
  val sequenceQueueViewL: Lens[SeqexecModelUpdate, SequencesQueue[SequenceView]] = Lens[SeqexecModelUpdate, SequencesQueue[SequenceView]](_.view)(q => {
      case e @ SequenceStart(_)               => e.copy(view = q)
      case e @ StepExecuted(_, _)             => e.copy(view = q)
      case e @ FileIdStepExecuted(_, _)       => e.copy(view = q)
      case e @ SequenceCompleted(_)           => e.copy(view = q)
      case e @ SequenceLoaded(_, _)           => e.copy(view = q)
      case e @ SequenceUnloaded(_, _)         => e.copy(view = q)
      case e @ StepBreakpointChanged(_)       => e.copy(view = q)
      case e @ OperatorUpdated(_)             => e.copy(view = q)
      case e @ ObserverUpdated(_)             => e.copy(view = q)
      case e @ ConditionsUpdated(_)           => e.copy(view = q)
      case e @ StepSkipMarkChanged(_)         => e.copy(view = q)
      case e @ SequencePauseRequested(_)      => e.copy(view = q)
      case e @ SequencePauseCanceled(_)       => e.copy(view = q)
      case e @ SequenceRefreshed(_, _)        => e.copy(view = q)
      case e @ ActionStopRequested(_)         => e.copy(view = q)
      case e @ ResourcesBusy(_, _, _)         => e.copy(view = q)
      case e @ SequenceError(_, _)            => e.copy(view = q)
      case e @ SequencePaused(_, _)           => e.copy(view = q)
      case e @ ExposurePaused(_, _)           => e.copy(view = q)
      case e @ SequenceUpdated(_)             => e.copy(view = q)
      case e @ LoadSequenceUpdated(_, _, _)   => e.copy(view = q)
      case e @ ClearLoadedSequencesUpdated(_) => e.copy(view = q)
      case e                                  => e
    }
  )

  val sequenceViewT: Traversal[SeqexecModelUpdate, SequenceView] =
    sequenceQueueViewL ^|->  // Find the sequence view
    sequencesQueueL    ^|->> // Find the queue
    eachViewT                // each sequence on the queue

  val sequenceStepT: Traversal[SequenceView, StandardStep] =
    obsStepsL          ^|->> // sequence steps
    eachStepT          ^<-?  // each step
    standardStepP            // which is a standard step

  // Composite lens to change the sequence name of an event
  val sequenceNameT: Traversal[SeqexecEvent, ObservationName] =
    sequenceEventsP    ^|->  // Events with model updates
    sequenceQueueViewL ^|->  // Find the sequence view
    sequencesQueueL    ^|->> // Find the queue
    eachViewT          ^|->  // each sequence on the queue
    obsNameL              // sequence's observation name

  // Composite lens to find the step config
  val sequenceConfigT: Traversal[SeqexecEvent, StepConfig] =
    sequenceEventsP    ^|->  // Events with model updates
    sequenceQueueViewL ^|->  // Find the sequence view
    sequencesQueueL    ^|->> // Find the queue
    eachViewT          ^|->  // each sequence on the queue
    obsStepsL          ^|->> // sequence steps
    eachStepT          ^<-?  // each step
    standardStepP      ^|->  // which is a standard step
    stepConfigL             // configuration of the step

  def filterEntry[K, V](predicate: (K, V) => Boolean): Traversal[Map[K, V], V] =
    new Traversal[Map[K, V], V]{
      def modifyF[F[_]: Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] =
        s.toList.traverse{ case (k, v) =>
          (if(predicate(k, v)) f(v) else v.pure[F]).tupleLeft(k)
        }.map(kvs => kvs.toMap)
    }

  // Find the Parameters of the steps containing science steps
  val scienceStepT: Traversal[StepConfig, Parameters] = filterEntry[SystemName, Parameters] {
    case (s, p) => s === SystemName.Observe && p.exists {
      case (k, v) => k === SystemName.Observe.withParam("observeType") && v === "OBJECT"
    }
  }

  val scienceTargetNameO: Optional[Parameters, TargetName] =
    paramValueL(SystemName.Observe.withParam("object")) ^<-? // find the target name
    some                                                     // focus on the option

  val stringToStepTypeP: Prism[String, StepType] = Prism(StepType.fromString)(_.show)
  private[model] def telescopeOffsetPI: Iso[Double, TelescopeOffset.P] = Iso(TelescopeOffset.P.apply)(_.value)
  private[model] def telescopeOffsetQI: Iso[Double, TelescopeOffset.Q] = Iso(TelescopeOffset.Q.apply)(_.value)
  val stringToDoubleP: Prism[String, Double] = Prism((x: String) => x.parseDouble.toOption)(_.show)
  val stringToIntP: Prism[String, Int] = Prism((x: String) => x.parseInt.toOption)(_.show)

  def stepObserveOptional[A](systemName: SystemName, param: String, prism: Prism[String, A]): Optional[Step, A] =
    standardStepP                            ^|-> // which is a standard step
    stepConfigL                              ^|-> // configuration of the step
    systemConfigL(systemName)                ^<-? // Observe config
    some                                     ^|-> // some
    paramValueL(systemName.withParam(param)) ^<-? // find the target name
    some                                     ^<-? // focus on the option
    prism                                         // step type

  val stepTypeO: Optional[Step, StepType] =
    stepObserveOptional(SystemName.Observe, "observeType", stringToStepTypeP)

  // Composite lens to find the observe exposure time
  val observeExposureTimeO: Optional[Step, Double] =
    stepObserveOptional(SystemName.Observe, "exposureTime", stringToDoubleP)

  // Composite lens to find the observe coadds
  val observeCoaddsO: Optional[Step, Int] =
    stepObserveOptional(SystemName.Observe, "coadds", stringToIntP)

  val stringToFPUModeP: Prism[String, FPUMode] = Prism(FPUMode.fromString)(_.show)
  // Composite lens to find the instrument fpu model
  val instrumentFPUModeO: Optional[Step, FPUMode] =
    stepObserveOptional(SystemName.Instrument, "fpuMode", stringToFPUModeP)

  // Composite lens to find the instrument fpu
  val instrumentFPUO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "fpu", Iso.id[String].asPrism)

  // Composite lens to find the instrument fpu custom mask
  val instrumentFPUCustomMaskO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "fpuCustomMask", Iso.id[String].asPrism)

  // Composite lens to find the instrument filter
  val instrumentFilterO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "filter", Iso.id[String].asPrism)

  // Composite lens to find the instrument disperser for GMOS
  val instrumentDisperserO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "disperser", Iso.id[String].asPrism)

  // Composite lens to find the instrument observing mode on GPI
  val instrumentObservingModeO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "observingMode", Iso.id[String].asPrism)

  // Composite lens to find the central wavelength for a disperser
  val instrumentDisperserLambdaO: Optional[Step, Double] =
    stepObserveOptional(SystemName.Instrument, "disperserLambda", stringToDoubleP)

  // Lens to find p offset
  def telescopeOffsetO(x: OffsetAxis): Optional[Step, Double] =
    stepObserveOptional(SystemName.Telescope, x.configItem, stringToDoubleP)

  val telescopeOffsetPO: Optional[Step, TelescopeOffset.P] = telescopeOffsetO(OffsetAxis.AxisP) ^<-> telescopeOffsetPI
  val telescopeOffsetQO: Optional[Step, TelescopeOffset.Q] = telescopeOffsetO(OffsetAxis.AxisQ) ^<-> telescopeOffsetQI

  val stringToGuidingP: Prism[String, Guiding] = Prism(Guiding.fromString)(_.configValue)

  // Lens to find guidingWith configurations
  val telescopeGuidingWithT: Traversal[Step, Guiding] =
    standardStepP                                                       ^|->  // which is a standard step
    stepConfigL                                                         ^|->  // configuration of the step
    systemConfigL(SystemName.Telescope)                                 ^<-?  // Observe config
    some                                                                ^|->> // some
    paramValuesWithPrefixT(SystemName.Telescope.withParam("guideWith")) ^<-?  // find the guiding with params
    stringToGuidingP                                                          // to guiding

  // Composite lens to find the step config
  val firstScienceTargetNameT: Traversal[SeqexecEvent, TargetName] =
    sequenceConfigT     ^|->> // sequence configuration
    scienceStepT        ^|-?  // science steps
    scienceTargetNameO        // science target name

  // Composite lens to find the target name on observation
  val observeTargetNameT: Traversal[SeqexecEvent, TargetName] =
    sequenceConfigT                                 ^|-?  // configuration of the step
    configParamValueO(SystemName.Observe, "object")       // on the configuration find the target name

  // Composite lens to find the target name on telescope
  val telescopeTargetNameT: Traversal[SeqexecEvent, TargetName] =
    sequenceConfigT                                       ^|-?  // configuration of the step
    configParamValueO(SystemName.Telescope, "Base:name")        // on the configuration find the target name

  // Composite lens to find the first science step and from there the target name
  val firstScienceStepTargetNameT: Traversal[SequenceView, TargetName] =
    obsStepsL           ^|->> // observation steps
    eachStepT           ^<-?  // each step
    standardStepP       ^|->  // only standard steps
    stepConfigL         ^|->> // get step config
    scienceStepT        ^|-?  // science steps
    scienceTargetNameO        // science target name

}
