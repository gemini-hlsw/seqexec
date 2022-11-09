// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.syntax.all._
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.optics.Format
import lucuma.core.syntax.all._
import monocle._
import monocle.function.At.atMap
import monocle.function.FilterIndex
import monocle.macros.GenLens
import monocle.macros.GenPrism
import monocle.std.option.some
import monocle.std.string._
import seqexec.model.enum._
import seqexec.model.events._

trait ModelLenses {
  // Some useful Monocle lenses
  val obsNameL: Lens[SequenceView, String]                                          =
    GenLens[SequenceView](_.metadata.name)
  val eachStepT: Traversal[List[Step], Step]                                        =
    Traversal.fromTraverse[List, Step]
  val obsStepsL: Lens[SequenceView, List[Step]]                                     = GenLens[SequenceView](_.steps)
  val eachViewT: Traversal[List[SequenceView], SequenceView]                        =
    Traversal.fromTraverse[List, SequenceView]
  val sessionQueueL: Lens[SequencesQueue[SequenceView], List[SequenceView]]         =
    GenLens[SequencesQueue[SequenceView]](_.sessionQueue)
  // Prism to focus on only the SeqexecEvents that have a queue
  val sequenceEventsP: Prism[SeqexecEvent, SeqexecModelUpdate]                      =
    GenPrism[SeqexecEvent, SeqexecModelUpdate]
  // Required for type correctness
  val stepConfigRoot: Iso[Map[SystemName, Parameters], Map[SystemName, Parameters]] =
    Iso.id[Map[SystemName, Parameters]]
  val parametersRoot: Iso[Map[ParamName, ParamValue], Map[ParamName, ParamValue]]   =
    Iso.id[Map[ParamName, ParamValue]]

  val sequenceStepT: Traversal[SequenceView, Step] =
    obsStepsL.andThen( // sequence steps
      eachStepT
    )                  // each step

  // Focus on a param value
  def paramValueL(param: ParamName): Lens[Parameters, Option[String]] =
    parametersRoot.andThen( // map of parameters
      atMap[ParamName, ParamValue].at(param)
    )                       // parameter containing the name

  // Focus on params with a prefix
  def paramValuesWithPrefixT(param: ParamName): Traversal[Parameters, String] =
    // parametersRoot andThen // map of parameters
    FilterIndex.mapFilterIndex[ParamName, ParamValue].filterIndex { n: ParamName =>
      n.startsWith(param)
    } // parameter containing the name

  // Possible set of observe parameters
  def systemConfigL(system: SystemName): Lens[StepConfig, Option[Parameters]] =
    stepConfigRoot.andThen( // map of systems
      atMap[SystemName, Parameters].at(system)
    )                       // subsystem name

  // Param name of a StepConfig
  def configParamValueO(
    system: SystemName,
    param:  String
  ): Optional[StepConfig, String] =
    systemConfigL(system)
      .andThen(      // observe parameters
        some[Parameters]
      )
      .andThen(      // focus on the option
        paramValueL(system.withParam(param))
      )
      .andThen(      // find the target name
        some[String] // focus on the option
      )

  // Focus on the sequence view
  val sequenceQueueViewL: Lens[SeqexecModelUpdate, SequencesQueue[SequenceView]] =
    Lens[SeqexecModelUpdate, SequencesQueue[SequenceView]](_.view)(q => {
      case e @ SequenceStart(_, _, _)          => e.copy(view = q)
      case e @ StepExecuted(_, _)              => e.copy(view = q)
      case e @ FileIdStepExecuted(_, _)        => e.copy(view = q)
      case e @ SequenceCompleted(_)            => e.copy(view = q)
      case e @ SequenceLoaded(_, _)            => e.copy(view = q)
      case e @ SequenceUnloaded(_, _)          => e.copy(view = q)
      case e @ StepBreakpointChanged(_)        => e.copy(view = q)
      case e @ OperatorUpdated(_)              => e.copy(view = q)
      case e @ ObserverUpdated(_)              => e.copy(view = q)
      case e @ ConditionsUpdated(_)            => e.copy(view = q)
      case e @ StepSkipMarkChanged(_)          => e.copy(view = q)
      case e @ SequencePauseRequested(_)       => e.copy(view = q)
      case e @ SequencePauseCanceled(_, _)     => e.copy(view = q)
      case e @ SequenceRefreshed(_, _)         => e.copy(view = q)
      case e @ ActionStopRequested(_)          => e.copy(view = q)
      case e @ SequenceError(_, _)             => e.copy(view = q)
      case e @ SequencePaused(_, _)            => e.copy(view = q)
      case e @ ExposurePaused(_, _)            => e.copy(view = q)
      case e @ SequenceUpdated(_)              => e.copy(view = q)
      case e @ LoadSequenceUpdated(_, _, _, _) => e.copy(view = q)
      case e @ ClearLoadedSequencesUpdated(_)  => e.copy(view = q)
      case e @ QueueUpdated(_, _)              => e.copy(view = q)
      case e                                   => e
    })

  val sequenceViewT: Traversal[SeqexecModelUpdate, SequenceView] =
    sequenceQueueViewL
      .andThen( // Find the sequence view
        sessionQueueL
      )
      .andThen( // Find the queue
        eachViewT
      )         // each sequence on the queue

  // Composite lens to change the sequence name of an event
  val sequenceNameT: Traversal[SeqexecEvent, ObservationName] =
    sequenceEventsP
      .andThen( // Events with model updates
        sequenceQueueViewL
      )
      .andThen( // Find the sequence view
        sessionQueueL
      )
      .andThen( // Find the queue
        eachViewT
      )
      .andThen( // each sequence on the queue
        obsNameL
      )         // sequence's observation name

  // Composite lens to find the step config
  val sequenceConfigT: Traversal[SeqexecEvent, StepConfig] =
    sequenceEventsP
      .andThen( // Events with model updates
        sequenceQueueViewL
      )
      .andThen( // Find the sequence view
        sessionQueueL
      )
      .andThen( // Find the queue
        eachViewT
      )
      .andThen( // each sequence on the queue
        obsStepsL
      )
      .andThen( // sequence steps
        eachStepT
      )
      .andThen( // each step
        Step.config
      )         // configuration of the step

  def filterEntry[K, V](predicate: (K, V) => Boolean): Traversal[Map[K, V], V] =
    new PTraversal[Map[K, V], Map[K, V], V, V] {
      override def modifyA[F[_]: Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] =
        s.toList
          .traverse { case (k, v) =>
            (if (predicate(k, v)) f(v) else v.pure[F]).tupleLeft(k)
          }
          .map(kvs => kvs.toMap)
    }

  // Find the Parameters of the steps containing science steps
  val scienceStepT: Traversal[StepConfig, Parameters] =
    filterEntry[SystemName, Parameters] { case (s, p) =>
      s === SystemName.Observe && p.exists { case (k, v) =>
        k === SystemName.Observe.withParam("observeType") && v === "OBJECT"
      }
    }

  val scienceTargetNameO: Optional[Parameters, TargetName] =
    paramValueL(SystemName.Observe.withParam("object")).andThen( // find the target name
      some[String]
    )                                                            // focus on the option

  val signedArcsecFormat: Format[String, Angle]                     =
    Format[String, BigDecimal](_.parseBigDecimalOption, _.toString)
      .andThen(Angle.signedDecimalArcseconds.reverse.asFormat)
  def signedComponentFormat[A]: Format[String, Offset.Component[A]] =
    signedArcsecFormat.andThen(Offset.Component.angle[A].reverse)

  val stringToDoubleP: Prism[String, Double] =
    Prism((x: String) => x.parseDoubleOption)(_.show)

  def stepObserveOptional[A](
    systemName: SystemName,
    param:      String,
    prism:      Prism[String, A]
  ): Optional[Step, A] =
    Step.config
      .andThen( // configuration of the step
        configParamValueO(systemName, param)
      )
      .andThen(prism) // step type

  val stringToString = Iso.id[String].asPrism

  val stringToStepTypeP: Prism[String, StepType] =
    Prism(StepType.fromString)(_.label)

  val stepTypeO: Optional[Step, StepType] =
    stepObserveOptional(SystemName.Observe, "observeType", stringToStepTypeP)

  // Composite lens to find the observe exposure time
  val observeExposureTimeO: Optional[Step, Double] =
    stepObserveOptional(SystemName.Observe, "exposureTime", stringToDoubleP)

  // Composite lens to find the observe coadds
  val observeCoaddsO: Optional[Step, Int] =
    stepObserveOptional(SystemName.Observe, "coadds", stringToInt)

  val stringToFPUModeP: Prism[String, FPUMode]    =
    Prism(FPUMode.fromString)(_.label)
  // Composite lens to find the instrument fpu model
  val instrumentFPUModeO: Optional[Step, FPUMode] =
    stepObserveOptional(SystemName.Instrument, "fpuMode", stringToFPUModeP)

  // Composite lens to find if the step is N&S
  val isNodAndShuffleO: Optional[Step, Boolean] =
    stepObserveOptional(SystemName.Instrument, "useNS", stringToBoolean)

  // Composite lens to find the instrument fpu
  val instrumentFPUO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "fpu", stringToString)

  // Composite lens to find the instrument slit width
  val instrumentSlitWidthO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "slitWidth", stringToString)

  // Composite lens to find the instrument fpu custom mask
  val instrumentFPUCustomMaskO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "fpuCustomMask", stringToString)

  // Composite lens to find the instrument filter
  val instrumentFilterO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "filter", stringToString)

  // Composite lens to find the instrument camera, e.g. Niri
  val instrumentCameraO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "camera", stringToString)

  // Composite lens to find the instrument disperser for GMOS
  val instrumentDisperserO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "disperser", stringToString)

  // Composite lens to find the instrument decker on GNIRS
  val instrumentDeckerO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "decker", stringToString)

  // Composite lens to find the instrument decker on GNIRS
  val instrumentImagingMirrorO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "imagingMirror", stringToString)

  // Instrument's mask
  val instrumentMaskO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "mask", stringToString)

  // Instrument's readMode
  val instrumentReadModeO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "readMode", stringToString)

  // Composite lens to find the instrument observing mode on GPI
  val instrumentObservingModeO: Optional[Step, String] =
    stepObserveOptional(SystemName.Instrument, "observingMode", stringToString)

  // Composite lens to find the central wavelength for a disperser
  val instrumentDisperserLambdaO: Optional[Step, Double] =
    stepObserveOptional(SystemName.Instrument, "disperserLambda", stringToDoubleP)

  // Lens to find offsets
  def offsetO[T, A](implicit resolver: OffsetConfigResolver[T, A]): Optional[Step, String] =
    stepObserveOptional(resolver.systemName, resolver.configItem, Iso.id[String])

  def offsetF[T, A](implicit
    resolver: OffsetConfigResolver[T, A]
  ): Fold[Step, Option[Offset.Component[A]]] =
    offsetO[T, A].andThen(Getter(signedComponentFormat[A].getOption))

  val stringToGuidingP: Prism[String, Guiding] =
    Prism(Guiding.fromString)(_.configValue)

  // Lens to find guidingWith configurations
  val telescopeGuidingWithT: Traversal[Step, Guiding] =
    Step.config
      .andThen(          // configuration of the step
        systemConfigL(SystemName.Telescope)
      )
      .andThen(          // Observe config
        some[Parameters]
      )
      .andThen(          // some
        paramValuesWithPrefixT(
          SystemName.Telescope.withParam("guideWith")
        )
      )
      .andThen(          // find the guiding with params
        stringToGuidingP // to guiding
      )

  // Composite lens to find the step config
  val firstScienceTargetNameT: Traversal[SeqexecEvent, TargetName] =
    sequenceConfigT
      .andThen( // sequence configuration
        scienceStepT
      )
      .andThen( // science steps
        scienceTargetNameO
      )         // science target name

  // Composite lens to find the sequence obs class
  val obsClassT: Traversal[SequenceView, String] =
    obsStepsL
      .andThen( // observation steps
        eachStepT
      )
      .andThen( // each step
        Step.config
      )
      .andThen( // get step config
        configParamValueO(SystemName.Observe, "class")
      )

  // Composite lens to find the sequence obs class
  val stepClassO: Optional[Step, String] =
    stepObserveOptional(SystemName.Observe, "class", Iso.id)

  // Composite lens to find the target name on observation
  val observeTargetNameT: Traversal[SeqexecEvent, TargetName] =
    sequenceConfigT.andThen( // configuration of the step
      configParamValueO(SystemName.Observe, "object")
    )                        // on the configuration find the target name

  // Composite lens to find the target name on telescope
  val telescopeTargetNameT: Traversal[SeqexecEvent, TargetName] =
    sequenceConfigT.andThen( // configuration of the step
      configParamValueO(SystemName.Telescope, "Base:name")
    )                        // on the configuration find the target name

  // Composite lens to find the first science step and from there the target name
  val firstScienceStepTargetNameT: Traversal[SequenceView, TargetName] =
    obsStepsL
      .andThen( // observation steps
        eachStepT
      )
      .andThen( // each step
        Step.config
      )
      .andThen( // only standard steps
        scienceStepT
      )
      .andThen( // science steps
        scienceTargetNameO
      )         // science target name

}

object ModelLenses extends ModelLenses
