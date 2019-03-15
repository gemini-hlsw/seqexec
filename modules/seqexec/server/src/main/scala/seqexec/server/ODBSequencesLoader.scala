// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Applicative
import cats.Monad
import cats.Endo
import cats.implicits._
import cats.effect.{ Concurrent, IO, Timer }
import edu.gemini.spModel.obscomp.InstConstants
import edu.gemini.spModel.seqcomp.SeqConfigNames.OCS_KEY
import edu.gemini.spModel.core.SPProgramID
import gem.Observation
import seqexec.engine.Event
import seqexec.engine.Sequence
import seqexec.server.ConfigUtilOps._

final class ODBSequencesLoader[F[_]: Monad](odbProxy: OdbProxy[F], translator: SeqTranslate) {
  private def unloadEvent(seqId: Observation.Id): executeEngine.EventType =
    Event.modifyState[executeEngine.ConcreteTypes](
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
    val t: F[Either[SeqexecFailure, (List[SeqexecFailure], Option[SequenceGen])]] =
      odbProxy.read(seqId).map {o =>
        for {
          odbSeq <- o
          progIdString <- odbSeq
                            .config
                            .extractAs[String](OCS_KEY / InstConstants.PROGRAMID_PROP)
                            .leftMap(ConfigUtilOps.explainExtractError)
          _            <- Either.catchNonFatal(
                            Applicative[F].pure(SPProgramID.toProgramID(progIdString)))
                          .leftMap(e => SeqexecFailure.SeqexecException(e): SeqexecFailure)
        } yield translator.sequence(seqId, odbSeq)
      }

    def loadSequenceEvent(seqg: SequenceGen): executeEngine.EventType =
      Event.modifyState[executeEngine.ConcreteTypes]({ st: EngineState =>
        st.sequences
          .get(seqId)
          .fold(ODBSequencesLoader.loadSequenceEndo(seqId, seqg))(
            _ => ODBSequencesLoader.reloadSequenceEndo(seqId, seqg)
          )(st)
      }.withEvent(LoadSequence(seqId)).toHandle)

    t.map {
      _.map {
        case (err :: _, None) =>
          List(Event.logDebugMsg(SeqexecFailure.explain(err)))
        case (errs, Some(seq)) =>
          loadSequenceEvent(seq) :: errs.map(e =>
            Event.logDebugMsg(SeqexecFailure.explain(e)))
        case _ => Nil
      }
      .valueOr(e => List(Event.logDebugMsg(SeqexecFailure.explain(e))))
    }
  }

  def refreshSequenceList(odbList: List[Observation.Id])(
    st: EngineState)(
      implicit cio: Concurrent[IO],
               tio: Timer[IO]
    ): F[List[executeEngine.EventType]] = {
    val seqexecList = st.sequences.keys.toList

    val loads = odbList.diff(seqexecList).map(loadEvents).sequence.map(_.flatten)

    val unloads = seqexecList.diff(odbList).map(unloadEvent)

    loads.map(_ ++ unloads)
  }

}

object ODBSequencesLoader {

  // TODO Parametrize the IO
  private def toEngineSequence(
    id:  Observation.Id,
    seq: SequenceGen,
    d:   HeaderExtraData
  ): Sequence[IO] = Sequence(id, toStepList(seq, d))

  private[server] def loadSequenceEndo(
    seqId: Observation.Id,
    seqg:  SequenceGen
  ): Endo[EngineState] =
    st =>
      EngineState.sequences.modify(
        ss =>
          ss + (seqId -> SequenceData(
            None,
            seqg,
            executeEngine.load(
              toEngineSequence(
                seqId,
                seqg,
                HeaderExtraData(st.conditions, st.operator, None))))))(st)

  private[server] def reloadSequenceEndo(
    seqId: Observation.Id,
    seqg:  SequenceGen
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
