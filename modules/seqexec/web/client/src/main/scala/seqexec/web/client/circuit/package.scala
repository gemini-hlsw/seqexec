// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import diode._
import gem.Observation
import monocle.Getter
import monocle.Lens
import monocle.macros.Lenses
import seqexec.model._
import seqexec.model.enum._
import seqexec.web.client.model.lenses.firstScienceStepTargetNameT
import seqexec.web.client.model._
import seqexec.web.client.model.ModelOps._

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

  import monocle.Optional

  /**
    * This lets us use monocle lenses to create diode ModelRW instances
    */
  class CircuitOps[M <: AnyRef](circuit: Circuit[M]) {
    def zoomRWL[A: Eq](lens: Lens[M, A]): ModelRW[M, A] =
      circuit.zoomRW(lens.get)((m, a) => lens.set(a)(m))(fastEq[A])

    def zoomL[A: Eq](lens: Lens[M, A]): ModelR[M, A] =
      circuit.zoom[A](lens.get)(fastEq[A])

    def zoomO[A: Eq](lens: Optional[M, A]): ModelR[M, Option[A]] =
      circuit.zoom[Option[A]](lens.getOption)(fastEq[Option[A]])

    def zoomG[A: Eq](getter: Getter[M, A]): ModelR[M, A] =
      circuit.zoom[A](getter.get)(fastEq[A])
  }

  // All these classes are focused views of the root model. They are used to only update small sections of the
  // UI even if other parts of the root model change
  @Lenses
  final case class SequencesFocus(sequences: SequencesQueue[SequenceView],
                                  sod:       SequencesOnDisplay)

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

  final case class SequenceInfoFocus(canOperate: Boolean,
                                     obsName:    String,
                                     status:     SequenceState,
                                     targetName: Option[TargetName])

  object SequenceInfoFocus {
    implicit val eq: Eq[SequenceInfoFocus] =
      Eq.by(x => (x.canOperate, x.obsName, x.status, x.targetName))

    def sequenceInfoG(
      id: Observation.Id
    ): Getter[SeqexecAppRootModel, Option[SequenceInfoFocus]] = {
      val getter =
        SeqexecAppRootModel.sequencesOnDisplayL.composeGetter(
          SequencesOnDisplay.tabG(id))
      ClientStatus.canOperateG.zip(getter) >>> {
        case (status, Some(SeqexecTabActive(tab, _))) =>
          val targetName =
            firstScienceStepTargetNameT.headOption(tab.sequence)
          SequenceInfoFocus(status,
                            tab.sequence.metadata.name,
                            tab.sequence.status,
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

    def statusAndStepG(
      id: Observation.Id
    ): Getter[SeqexecAppRootModel, Option[StatusAndStepFocus]] = {
      val getter =
        SeqexecAppRootModel.sequencesOnDisplayL.composeGetter(
          SequencesOnDisplay.tabG(id))
      ClientStatus.canOperateG.zip(getter) >>> {
        case (canOperate, st) =>
          st.map {
            case SeqexecTabActive(tab, _) =>
              StatusAndStepFocus(canOperate,
                                 tab.sequence.metadata.instrument,
                                 tab.obsId,
                                 tab.stepConfigDisplayed,
                                 tab.sequence.steps.length,
                                 tab.isPreview)
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

  object ControlModel {
    implicit val eq: Eq[ControlModel] =
      Eq.by(
        x =>
          (x.id,
           x.isPartiallyExecuted,
           x.nextStepToRun,
           x.status,
           x.tabOperations))

    val controlModelG: Getter[SequenceTab, ControlModel] =
      Getter[SequenceTab, ControlModel](
        t =>
          ControlModel(t.obsId,
                       t.sequence.isPartiallyExecuted,
                       t.sequence.nextStepToRun,
                       t.sequence.status,
                       t.tabOperations))
  }

  @Lenses
  final case class SequenceControlFocus(canOperate: Boolean,
                                        control:    ControlModel)

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
