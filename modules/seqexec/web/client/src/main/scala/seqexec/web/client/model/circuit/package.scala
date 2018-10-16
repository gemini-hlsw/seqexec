// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.Eq
import cats.Order
import cats.implicits._
import cats.data.NonEmptyList
import diode._
import gem.Observation
import monocle.Getter
import monocle.Lens
import monocle.macros.Lenses
import seqexec.model._
import seqexec.model.enum._
import seqexec.web.client.lenses.firstScienceStepTargetNameT
import seqexec.web.client.model._
import seqexec.web.client.ModelOps._
import seqexec.web.client.components.sequence.steps.StepsTable
import seqexec.web.client.components.SessionQueueTableBody
import web.client.table._

package object circuit {
  implicit def CircuitToOps[T <: AnyRef](c: Circuit[T]): CircuitOps[T] =
    new CircuitOps(c)

  implicit def fastEq[A: Eq]: FastEq[A] = new FastEq[A] {
    override def eqv(a: A, b: A): Boolean = a === b
  }

  implicit def fastNelEq[A: Eq]: FastEq[NonEmptyList[A]] =
    new FastEq[NonEmptyList[A]] {
      override def eqv(a: NonEmptyList[A], b: NonEmptyList[A]): Boolean =
        a === b
    }
}

package circuit {

  /**
    * This lets us use monocle lenses to create diode ModelRW instances
    */
  class CircuitOps[M <: AnyRef](circuit: Circuit[M]) {
    def zoomRWL[A: Eq](lens: Lens[M, A]): ModelRW[M, A] =
      circuit.zoomRW(lens.get)((m, a) => lens.set(a)(m))(fastEq[A])

    def zoomL[A: Eq](lens: Lens[M, A]): ModelR[M, A] =
      circuit.zoom[A](lens.get)(fastEq[A])

    def zoomG[A: Eq](getter: Getter[M, A]): ModelR[M, A] =
      circuit.zoom[A](getter.get)(fastEq[A])
  }

  // All these classes are focused views of the root model. They are used to only update small sections of the
  // UI even if other parts of the root model change
  @Lenses
  final case class SequencesFocus(sequences: SequencesQueue[SequenceView],
                                  sod:       SequencesOnDisplay)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object SequencesFocus {
    implicit val eq: Eq[SequencesFocus] =
      Eq.by(x => (x.sequences, x.sod))

    val sequencesFocusL: Lens[SeqexecAppRootModel, SequencesFocus] =
      Lens[SeqexecAppRootModel, SequencesFocus](m =>
        SequencesFocus(m.sequences, m.uiModel.sequencesOnDisplay))(
        v =>
          m =>
            m.copy(sequences = v.sequences,
                   uiModel   = m.uiModel.copy(sequencesOnDisplay = v.sod)))

  }

  @Lenses
  final case class SODLocationFocus(location: Pages.SeqexecPages,
                                    sod:      SequencesOnDisplay,
                                    clientId: Option[ClientId])

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object SODLocationFocus {
    implicit val eq: Eq[SODLocationFocus] =
      Eq.by(x => (x.location, x.sod, x.clientId))

    val sodLocationFocusL: Lens[SeqexecAppRootModel, SODLocationFocus] =
      Lens[SeqexecAppRootModel, SODLocationFocus](
        m =>
          SODLocationFocus(m.uiModel.navLocation,
                           m.uiModel.sequencesOnDisplay,
                           m.clientId))(
        v =>
          m =>
            m.copy(clientId = v.clientId,
                   uiModel = m.uiModel.copy(navLocation = v.location,
                                            sequencesOnDisplay = v.sod)))
  }

  @Lenses
  final case class InitialSyncFocus(location:  Pages.SeqexecPages,
                                    sod:       SequencesOnDisplay,
                                    firstLoad: Boolean)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object InitialSyncFocus {
    implicit val eq: Eq[InitialSyncFocus] =
      Eq.by(x => (x.location, x.sod, x.firstLoad))

    val initialSyncFocusL: Lens[SeqexecUIModel, InitialSyncFocus] =
      Lens[SeqexecUIModel, InitialSyncFocus](m =>
        InitialSyncFocus(m.navLocation, m.sequencesOnDisplay, m.firstLoad))(
        v =>
          m =>
            m.copy(navLocation        = v.location,
                   sequencesOnDisplay = v.sod,
                   firstLoad          = v.firstLoad))
  }

  final case class SequenceInSessionQueue(id:            Observation.Id,
                                          status:        SequenceState,
                                          instrument:    Instrument,
                                          active:        Boolean,
                                          loaded:        Boolean,
                                          name:          String,
                                          targetName:    Option[TargetName],
                                          runningStep:   Option[RunningStep],
                                          nextStepToRun: Option[Int])
      extends UseValueEq

  object SequenceInSessionQueue {
    implicit val order: Order[SequenceInSessionQueue] = Order.by(_.id)
    implicit val ordering: scala.math.Ordering[SequenceInSessionQueue] =
      order.toOrdering
  }

  final case class StatusAndLoadedSequencesFocus(
    status:     ClientStatus,
    sequences:  List[SequenceInSessionQueue],
    tableState: TableState[SessionQueueTableBody.TableColumn])
      extends UseValueEq

  final case class InstrumentStatusFocus(
    instrument:  Instrument,
    active:      Boolean,
    idState:     Option[(Observation.Id, SequenceState)],
    runningStep: Option[RunningStep])
      extends UseValueEq

  final case class TabFocus(
    canOperate:      Boolean,
    tabs:            NonEmptyList[Either[CalibrationQueueTabActive, AvailableTab]],
    defaultObserver: Observer)

  object TabFocus {
    implicit val eq: Eq[TabFocus] =
      Eq.by(x => (x.canOperate, x.tabs, x.defaultObserver))
  }

  sealed trait TabContentFocus extends Product with Serializable {
    val canOperate: Boolean
    val logDisplayed: SectionVisibilityState
    val active: TabSelected
  }

  object TabContentFocus {
    implicit val eq: Eq[TabContentFocus] =
      Eq.instance {
        case (a: SequenceTabContentFocus, b: SequenceTabContentFocus) => a === b
        case (a: CalQueueTabContentFocus, b: CalQueueTabContentFocus) => a === b
        case _                                                        => false
      }
  }

  final case class SequenceTabContentFocus(canOperate:   Boolean,
                                           instrument:   Option[Instrument],
                                           id:           Option[Observation.Id],
                                           completed:    Boolean,
                                           active:       TabSelected,
                                           logDisplayed: SectionVisibilityState)
      extends TabContentFocus

  object SequenceTabContentFocus {
    implicit val eq: Eq[SequenceTabContentFocus] =
      Eq.by(
        x =>
          (x.canOperate,
           x.instrument,
           x.id,
           x.completed,
           x.active,
           x.logDisplayed))
  }

  final case class CalQueueTabContentFocus(canOperate:   Boolean,
                                           active:       TabSelected,
                                           logDisplayed: SectionVisibilityState)
      extends TabContentFocus

  object CalQueueTabContentFocus {
    implicit val eq: Eq[CalQueueTabContentFocus] =
      Eq.by(x => (x.canOperate, x.active, x.logDisplayed))
  }

  final case class SequenceInfoFocus(canOperate: Boolean,
                                     obsName:    Option[String],
                                     status:     Option[SequenceState],
                                     targetName: Option[TargetName])
      extends UseValueEq

  object SequenceInfoFocus {
    implicit val eq: Eq[SequenceInfoFocus] =
      Eq.by(x => (x.canOperate, x.obsName, x.status, x.targetName))

    def sequenceInfoG(id: Observation.Id)
      : Getter[SeqexecAppRootModel, Option[SequenceInfoFocus]] = {
      val getter =
        SeqexecAppRootModel.sequencesOnDisplayL.composeGetter(
          SequencesOnDisplay.tabG(id))
      ClientStatus.canOperateG.zip(getter) >>> {
        case (status, Some(SeqexecTabActive(tab, _))) =>
          val targetName =
            tab.sequence.flatMap(firstScienceStepTargetNameT.headOption)
          SequenceInfoFocus(status,
                            tab.sequence.map(_.metadata.name),
                            tab.sequence.map(_.status),
                            targetName).some
        case _ => none
      }
    }
  }

  final case class StatusAndStepFocus(canOperate:          Boolean,
                                      instrument:          Instrument,
                                      obsId:               Observation.Id,
                                      stepConfigDisplayed: Option[Int],
                                      totalSteps:          Int,
                                      isPreview:           Boolean)

  object StatusAndStepFocus {
    implicit val eq: Eq[StatusAndStepFocus] =
      Eq.by(
        x =>
          (x.canOperate,
           x.instrument,
           x.obsId,
           x.stepConfigDisplayed,
           x.totalSteps,
           x.isPreview))

    def statusAndStepG(id: Observation.Id)
      : Getter[SeqexecAppRootModel, Option[StatusAndStepFocus]] = {
      val getter =
        SeqexecAppRootModel.sequencesOnDisplayL.composeGetter(
          SequencesOnDisplay.tabG(id))
      ClientStatus.canOperateG.zip(getter) >>> {
        case (canOperate, st) =>
          st.flatMap {
            case SeqexecTabActive(tab, _) =>
              tab.sequence.map { t =>
                StatusAndStepFocus(canOperate,
                                   t.metadata.instrument,
                                   t.id,
                                   tab.stepConfigDisplayed,
                                   t.steps.length,
                                   tab.isPreview)
              }
          }
      }
    }
  }

  final case class StepsTableFocus(
    id:                  Observation.Id,
    instrument:          Instrument,
    state:               SequenceState,
    steps:               List[Step],
    stepConfigDisplayed: Option[Int],
    nextStepToRun:       Option[Int],
    isPreview:           Boolean,
    tableState:          TableState[StepsTable.TableColumn])

  object StepsTableFocus {
    implicit val eq: Eq[StepsTableFocus] =
      Eq.by(
        x =>
          (x.id,
           x.instrument,
           x.state,
           x.steps,
           x.stepConfigDisplayed,
           x.nextStepToRun,
           x.isPreview,
           x.tableState))

    def stepsTableG(id: Observation.Id)
      : Getter[SeqexecAppRootModel, Option[StepsTableFocus]] =
      SeqexecAppRootModel.sequencesOnDisplayL.composeGetter(
        SequencesOnDisplay.tabG(id)) >>> {
        _.flatMap {
          case SeqexecTabActive(tab, _) =>
            tab.sequence.map { sequence =>
              StepsTableFocus(sequence.id,
                              sequence.metadata.instrument,
                              sequence.status,
                              sequence.steps,
                              tab.stepConfigDisplayed,
                              sequence.nextStepToRun,
                              tab.isPreview,
                              tab.tableState)
            }
        }
      }
  }

  @Lenses
  final case class ControlModel(id:                  Observation.Id,
                                isPartiallyExecuted: Boolean,
                                nextStepToRun:       Option[Int],
                                status:              SequenceState,
                                tabOperations:       TabOperations)

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object ControlModel {
    implicit val eq: Eq[ControlModel] =
      Eq.by(
        x =>
          (x.id,
           x.isPartiallyExecuted,
           x.nextStepToRun,
           x.status,
           x.tabOperations))

    val controlModelG: Getter[SequenceTab, Option[ControlModel]] =
      Getter[SequenceTab, Option[ControlModel]](
        t =>
          t.sequence.map(
            s =>
              ControlModel(s.id,
                           s.isPartiallyExecuted,
                           s.nextStepToRun,
                           s.status,
                           t.tabOperations)))
  }

  @Lenses
  final case class SequenceControlFocus(canOperate: Boolean,
                                        control:    Option[ControlModel])

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object SequenceControlFocus {
    implicit val eq: Eq[SequenceControlFocus] =
      Eq.by(x => (x.canOperate, x.control))

    def seqControlG(id: Observation.Id)
      : Getter[SeqexecAppRootModel, Option[SequenceControlFocus]] = {
      val getter = SeqexecAppRootModel.sequencesOnDisplayL.composeGetter(
        SequencesOnDisplay.tabG(id))
      ClientStatus.canOperateG.zip(getter) >>> {
        case (status, Some(SeqexecTabActive(tab, _))) =>
          SequenceControlFocus(status, ControlModel.controlModelG.get(tab)).some
        case _ => none
      }
    }
  }

}
