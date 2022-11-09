// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.ApplicativeError
import cats.Endo
import cats.effect.Async
import cats.syntax.all._
import edu.gemini.spModel.core.SPProgramID
import edu.gemini.spModel.obscomp.InstConstants
import edu.gemini.spModel.seqcomp.SeqConfigNames.OCS_KEY
import seqexec.engine.Event
import seqexec.engine.Sequence
import seqexec.model.Observation
import seqexec.model.SystemOverrides
import ConfigUtilOps._
import SeqEvent._
import SeqexecFailure.SeqexecException
import SeqexecFailure.UnrecognizedInstrument

final class ODBSequencesLoader[F[_]: Async](
  odbProxy:            OdbProxy[F],
  translator:          SeqTranslate[F]
)(implicit execEngine: ExecEngineType[F]) {

  private def unloadEvent(seqId: Observation.Id): EventType[F] =
    Event.modifyState[F, EngineState[F], SeqEvent](
      { st: EngineState[F] =>
        if (execEngine.canUnload(seqId)(st)) {
          (EngineState.sequences[F].modify(ss => ss - seqId) >>>
            EngineState.selected.modify(ss =>
              ss.toList.filter { case (_, x) => x =!= seqId }.toMap
            ) >>>
            EngineState.queues.modify(
              _.view.mapValues(ExecutionQueue.queue.modify(_.filterNot(_ === seqId))).toMap
            ))(st)
        } else st
      }.withEvent(UnloadSequence(seqId)).toHandle
    )

  def loadEvents(seqId: Observation.Id): F[List[EventType[F]]] = {
    // Three ways of handling errors are mixed here: java exceptions, Either and MonadError
    val t: F[(List[Throwable], Option[SequenceGen[F]])] =
      odbProxy.read(seqId).flatMap { odbSeq =>
        val configObsId: F[String] =
          odbSeq.config
            .extractAs[String](OCS_KEY / InstConstants.PROGRAMID_PROP)
            .toF[F]

        // Verify that the program id is valid
        configObsId
          .adaptErr { case _ => SeqexecFailure.Unexpected(s"Invalid $configObsId") }
          .flatMap(s => ApplicativeError[F, Throwable].catchNonFatal(SPProgramID.toProgramID(s)))
          .flatMap(_ => translator.sequence(seqId, odbSeq))
      }

    def loadSequenceEvent(seqg: SequenceGen[F]): EventType[F] =
      Event.modifyState[F, EngineState[F], SeqEvent]({ st: EngineState[F] =>
        st.sequences
          .get(seqId)
          .fold(ODBSequencesLoader.loadSequenceEndo(seqId, seqg, execEngine))(_ =>
            ODBSequencesLoader.reloadSequenceEndo(seqId, seqg, execEngine)
          )(st)
      }.withEvent(LoadSequence(seqId)).toHandle)

    t.map {
      case (UnrecognizedInstrument(_) :: _, None) =>
        Nil
      case (err :: _, None)                       =>
        val explanation = explain(err)
        List(Event.logDebugMsgF[F, EngineState[F], SeqEvent](explanation))
      case (errs, Some(seq))                      =>
        loadSequenceEvent(seq).pure[F] :: errs.map(e =>
          Event.logDebugMsgF[F, EngineState[F], SeqEvent](explain(e))
        )
      case _                                      => Nil
    }.recover { case e => List(Event.logDebugMsgF(explain(e))) }
      .map(_.sequence)
      .flatten
  }

  private def explain(err: Throwable): String =
    err match {
      case s: SeqexecFailure => SeqexecFailure.explain(s)
      case _                 => SeqexecFailure.explain(SeqexecException(err))
    }

  def refreshSequenceList(
    odbList: List[Observation.Id],
    st:      EngineState[F]
  ): F[List[EventType[F]]] = {
    val seqexecList = st.sequences.keys.toList

    val loads = odbList.diff(seqexecList).traverse(loadEvents).map(_.flatten)

    val unloads = seqexecList.diff(odbList).map(unloadEvent)

    loads.map(_ ++ unloads)
  }

}

object ODBSequencesLoader {

  private def toEngineSequence[F[_]](
    id:        Observation.Id,
    overrides: SystemOverrides,
    seq:       SequenceGen[F],
    d:         HeaderExtraData
  ): Sequence[F] = Sequence(id, toStepList(seq, overrides, d))

  private[server] def loadSequenceEndo[F[_]](
    seqId:      Observation.Id,
    seqg:       SequenceGen[F],
    execEngine: ExecEngineType[F]
  ): Endo[EngineState[F]] =
    st =>
      EngineState
        .sequences[F]
        .modify(ss =>
          ss + (seqId -> SequenceData[F](
            None,
            SystemOverrides.AllEnabled,
            seqg,
            execEngine.load(
              toEngineSequence(
                seqId,
                SystemOverrides.AllEnabled,
                seqg,
                HeaderExtraData(st.conditions, st.operator, None)
              )
            ),
            none
          ))
        )(st)

  private[server] def reloadSequenceEndo[F[_]](
    seqId:      Observation.Id,
    seqg:       SequenceGen[F],
    execEngine: ExecEngineType[F]
  ): Endo[EngineState[F]] =
    st =>
      EngineState
        .atSequence[F](seqId)
        .modify(sd =>
          sd.copy(
            seqGen = seqg,
            seq = execEngine.reload(
              sd.seq,
              toStepList(
                seqg,
                sd.overrides,
                HeaderExtraData(st.conditions, st.operator, sd.observer)
              )
            )
          )
        )(st)

}
