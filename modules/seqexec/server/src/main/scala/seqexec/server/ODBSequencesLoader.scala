// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.ApplicativeError
import cats.Endo
import cats.data.Nested
import cats.implicits._
import cats.effect.{ Concurrent, IO, Timer }
import edu.gemini.spModel.obscomp.InstConstants
import edu.gemini.spModel.seqcomp.SeqConfigNames.OCS_KEY
import edu.gemini.spModel.core.SPProgramID
import gem.Observation
import seqexec.engine.Event
import seqexec.engine.Sequence
import seqexec.server.SeqEvent._
import seqexec.server.ConfigUtilOps._

final class ODBSequencesLoader[F[_]: ApplicativeError[?[_], Throwable]](odbProxy: OdbProxy[F], translator: SeqTranslate) {
  private def unloadEvent(seqId: Observation.Id): executeEngine.EventType =
    Event.modifyState[IO, executeEngine.ConcreteTypes](
      { st: EngineState =>
        if (executeEngine.canUnload(seqId)(st)) {
          (EngineState.sequences.modify(ss => ss - seqId) >>>
            EngineState.selected.modify(ss =>
              ss.toList.filter { case (_, x) => x =!= seqId }.toMap) >>>
            EngineState.queues.modify(_.mapValues(
              ExecutionQueue.queue.modify(_.filterNot(_ === seqId)))))(st)
        } else st
      }.withEvent(UnloadSequence(seqId)).toHandle
    )

  def loadEvents(
    seqId: Observation.Id)(
    implicit cio: Concurrent[IO],
             tio: Timer[IO]
  ): F[List[executeEngine.EventType]] = {
    val t: F[Either[Throwable, (List[SeqexecFailure], Option[SequenceGen[IO]])]] =
      odbProxy.read(seqId).map {odbSeq =>
        val configObsId: Either[SeqexecFailure, String] =
          odbSeq
            .config
            .extractAs[String](OCS_KEY / InstConstants.PROGRAMID_PROP)
            .leftMap(ConfigUtilOps.explainExtractError)
        // Verify that the program id is valid
        configObsId
          .ensure(SeqexecFailure.Unexpected(s"Invalid $configObsId"))(
            s => Either.catchNonFatal(SPProgramID.toProgramID(s)).isRight).as {
          translator.sequence(seqId, odbSeq)
        }
      }.attempt.map(_.flatten)

    def loadSequenceEvent(seqg: SequenceGen[IO]): executeEngine.EventType =
      Event.modifyState[IO, executeEngine.ConcreteTypes]({ st: EngineState =>
        st.sequences
          .get(seqId)
          .fold(ODBSequencesLoader.loadSequenceEndo(seqId, seqg))(
            _ => ODBSequencesLoader.reloadSequenceEndo(seqId, seqg)
          )(st)
      }.withEvent(LoadSequence(seqId)).toHandle)

    Nested(t).map {
      case (err :: _, None) =>
        List(Event.logDebugMsg(SeqexecFailure.explain(err)))
      case (errs, Some(seq)) =>
        loadSequenceEvent(seq) :: errs.map(e =>
          Event.logDebugMsg(SeqexecFailure.explain(e)))
      case _ => Nil
    }.value.map(_.valueOr {
      case e: SeqexecFailure => List(Event.logDebugMsg(SeqexecFailure.explain(e)))
      case e: Throwable => List(Event.logDebugMsg(e.getMessage))
    })
  }

  def refreshSequenceList(odbList: List[Observation.Id], st: EngineState)(
      implicit cio: Concurrent[IO],
               tio: Timer[IO]
    ): F[List[executeEngine.EventType]] = {
    val seqexecList = st.sequences.keys.toList

    val loads = odbList.diff(seqexecList).traverse(loadEvents).map(_.flatten)

    val unloads = seqexecList.diff(odbList).map(unloadEvent)

    loads.map(_ ++ unloads)
  }

}

object ODBSequencesLoader {

  private def toEngineSequence[F[_]](
    id:  Observation.Id,
    seq: SequenceGen[F],
    d:   HeaderExtraData
  ): Sequence[F] = Sequence(id, toStepList(seq, d))

  private[server] def loadSequenceEndo(
    seqId: Observation.Id,
    seqg:  SequenceGen[IO]
  ): Endo[EngineState] =
    st =>
      EngineState.sequences.modify(
        ss =>
          ss + (seqId -> SequenceData[IO](
            None,
            seqg,
            executeEngine.load(
              toEngineSequence(
                seqId,
                seqg,
                HeaderExtraData(st.conditions, st.operator, None))))))(st)

  private[server] def reloadSequenceEndo(
    seqId: Observation.Id,
    seqg:  SequenceGen[IO]
  ): Endo[EngineState] =
    st =>
      EngineState
        .atSequence(seqId)
        .modify(
          sd =>
            sd.copy(
              seqGen = seqg,
              seq = executeEngine.reload(
                sd.seq,
                toStepList(
                  seqg,
                  HeaderExtraData(st.conditions, st.operator, sd.observer)))))(st)

}
