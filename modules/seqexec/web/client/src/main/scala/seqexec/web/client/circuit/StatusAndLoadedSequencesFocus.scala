// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.syntax.all._
import monocle.Getter
import seqexec.model.Observation
import seqexec.model._
import seqexec.model.enum._
import seqexec.web.client.components.SessionQueueTable
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.model._
import seqexec.web.client.model.lenses.firstScienceStepTargetNameT
import seqexec.web.client.model.lenses.obsClassT
import web.client.table._

final case class SequenceInSessionQueue(id:            Observation.Id,
                                        status:        SequenceState,
                                        instrument:    Instrument,
                                        active:        Boolean,
                                        loaded:        Boolean,
                                        name:          String,
                                        obsClass:      ObsClass,
                                        targetName:    Option[TargetName],
                                        runningStep:   Option[RunningStep],
                                        nextStepToRun: Option[Int],
                                        inDayCalQueue: Boolean)

object SequenceInSessionQueue {
  implicit val eq: Eq[SequenceInSessionQueue] =
    Eq.by(
      x =>
        (x.id,
         x.status,
         x.instrument,
         x.active,
         x.loaded,
         x.name,
         x.obsClass,
         x.targetName,
         x.runningStep,
         x.nextStepToRun,
         x.inDayCalQueue))

  def toSequenceInSessionQueue(
    sod:    SequencesOnDisplay,
    queue:  List[SequenceView],
    dayCal: List[Observation.Id]
  ): List[SequenceInSessionQueue] =
    queue.map { s =>
      val active     = sod.idDisplayed(s.id)
      val loaded     = sod.loadedIds.contains(s.id)
      val targetName = firstScienceStepTargetNameT.headOption(s)
      val obsClass = obsClassT
        .headOption(s)
        .map(ObsClass.fromString)
        .getOrElse(ObsClass.Nighttime)
      SequenceInSessionQueue(s.id,
                             s.status,
                             s.metadata.instrument,
                             active,
                             loaded,
                             s.metadata.name,
                             obsClass,
                             targetName,
                             s.runningStep,
                             s.nextStepToRun,
                             dayCal.contains(s.id))
    }

}

final case class StatusAndLoadedSequencesFocus(
  status:      ClientStatus,
  sequences:   List[SequenceInSessionQueue],
  tableState:  TableState[SessionQueueTable.TableColumn],
  queueFilter: SessionQueueFilter)

object StatusAndLoadedSequencesFocus {
  implicit val eq: Eq[StatusAndLoadedSequencesFocus] =
    Eq.by(x => (x.status, x.sequences, x.tableState, x.queueFilter))

  private val sessionQueueG = SeqexecAppRootModel.sessionQueueL.asGetter
  private val sessionQueueFilterG =
    SeqexecAppRootModel.sessionQueueFilterL.asGetter
  private val sodG = SeqexecAppRootModel.sequencesOnDisplayL.asGetter

  val statusAndLoadedSequencesG
    : Getter[SeqexecAppRootModel, StatusAndLoadedSequencesFocus] =
    ClientStatus.clientStatusFocusL.asGetter.zip(
      sessionQueueG.zip(sodG.zip(SeqexecAppRootModel.sessionQueueTableStateL.asGetter
        .zip(sessionQueueFilterG.zip(SeqexecAppRootModel.dayCalG))))) >>> {
      case (s, (queue, (sod, (queueTable, (filter, dayCal))))) =>
        StatusAndLoadedSequencesFocus(
          s,
          SequenceInSessionQueue
            .toSequenceInSessionQueue(sod, queue, dayCal.foldMap(_.queue))
            .sortBy(_.id),
          queueTable,
          filter)
    }

  val filteredSequencesG
    : Getter[SeqexecAppRootModel, List[SequenceInSessionQueue]] = {
    sessionQueueFilterG.zip(
      sessionQueueG.zip(sodG.zip(SeqexecAppRootModel.dayCalG))) >>> {
      case (f, (s, (sod, dayCal))) =>
        f.filter(
          SequenceInSessionQueue
            .toSequenceInSessionQueue(sod, s, dayCal.foldMap(_.queue)))
    }
  }

}
