// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Getter
import seqexec.model._
import seqexec.model.enum._
import seqexec.web.client.model._
import seqexec.web.client.model.lenses.firstScienceStepTargetNameT
import seqexec.web.client.model.lenses.obsClassT
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.components.SessionQueueTable
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
                                        nextStepToRun: Option[Int])

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
         x.nextStepToRun))
}

final case class StatusAndLoadedSequencesFocus(
  status:      ClientStatus,
  sequences:   List[SequenceInSessionQueue],
  tableState:  TableState[SessionQueueTable.TableColumn],
  queueFilter: SessionQueueFilter)

object StatusAndLoadedSequencesFocus {
  implicit val eq: Eq[StatusAndLoadedSequencesFocus] =
    Eq.by(x => (x.status, x.sequences, x.tableState, x.queueFilter))

  val statusAndLoadedSequencesG
    : Getter[SeqexecAppRootModel, StatusAndLoadedSequencesFocus] =
    ClientStatus.clientStatusFocusL.asGetter.zip(
      SeqexecAppRootModel.sessionQueueL.asGetter.zip(
        SeqexecAppRootModel.sequencesOnDisplayL.asGetter.zip(
          SeqexecAppRootModel.queueTableStateL.asGetter
            .zip(SeqexecAppRootModel.sessionQueueFilterL.asGetter)))) >>> {
      case (s, (queue, (sod, (queueTable, filter)))) =>
        val sequencesInQueue = queue.map { s =>
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
                                 s.nextStepToRun)
        }
        StatusAndLoadedSequencesFocus(s,
                                      sequencesInQueue.sortBy(_.id),
                                      queueTable,
                                      filter)
    }
}
