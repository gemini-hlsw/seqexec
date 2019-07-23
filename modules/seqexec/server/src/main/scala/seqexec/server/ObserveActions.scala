// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.effect.Sync
import cats.effect.Concurrent
import cats.data.Reader
import cats.data.EitherT
import cats.implicits._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import fs2.Stream
import gem.Observation
import org.log4s._
import seqexec.engine._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords._
import seqexec.server.InstrumentSystem.ElapsedTime
import squants.time.TimeConversions._

/**
 * Methods usedd to generate observation related actions
 */
trait ObserveActions {
  private val Log = getLogger

  // All instruments ask the DHS for an ImageFileId
  private def dhsFileId[F[_]: ApplicativeError[?[_], Throwable]](systems: Systems[F], inst: InstrumentSystem[F]): F[ImageFileId] =
    systems.dhs.createImage(DhsClient.ImageParameters(DhsClient.Permanent, List(inst.contributorName, "dhs-http")))

  private def info[F[_]: Sync](msg: => String): F[Unit] = Sync[F].delay(Log.info(msg))

  private def abortTail[F[_]: MonadError[?[_], Throwable]](systems: Systems[F], obsId: Observation.Id, imageFileId: ImageFileId): F[Result[F]] =
    systems.odb.obsAbort(obsId, imageFileId)
      .ensure(SeqexecFailure.Unexpected("Unable to send ObservationAborted message to ODB."))(identity) *>
    MonadError[F, Throwable].raiseError(SeqexecFailure.Execution(s"Observation ${obsId.format} aborted by user."))

  private def sendDataStart[F[_]: MonadError[?[_], Throwable]](systems: Systems[F], obsId: Observation.Id, imageFileId: ImageFileId, dataId: String): F[Unit] =
    systems.odb.datasetStart(obsId, dataId, imageFileId)
      .ensure(SeqexecFailure.Unexpected("Unable to send DataStart message to ODB."))(identity)
      .void

  private def sendDataEnd[F[_]: MonadError[?[_], Throwable]](systems: Systems[F], obsId: Observation.Id, imageFileId: ImageFileId, dataId: String): F[Unit] =
    systems.odb.datasetComplete(obsId, dataId, imageFileId)
      .ensure(SeqexecFailure.Unexpected("Unable to send DataEnd message to ODB."))(identity)
      .void

  def observe[F[_]: Sync: Concurrent](
    systems: Systems[F],
    config: Config,
    obsId: Observation.Id,
    inst: InstrumentSystem[F],
    otherSys: List[System[F]],
    headers: Reader[HeaderExtraData, List[Header[F]]])(
      ctx: HeaderExtraData): Stream[F, Result[F]]
  = {
    def dataId: F[String] =
      EitherT.fromEither[F](
        config.extractAs[String](OBSERVE_KEY / DATA_LABEL_PROP)
          .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
      ).widenRethrowT

    def notifyObserveStart: F[Unit] =
      otherSys.map(_.notifyObserveStart).sequence.void

    // endObserve must be sent to the instrument too.
    def notifyObserveEnd: F[Unit] =
      (inst +: otherSys).map(_.notifyObserveEnd).sequence.void

    def closeImage(id: ImageFileId): F[Unit] =
      inst.keywordsClient.closeImage(id)

    def doObserve(fileId: ImageFileId): F[Result[F]] =
      for {
        d   <- dataId
        _   <- sendDataStart(systems, obsId, fileId, d)
        _   <- notifyObserveStart
        _   <- headers(ctx).map(_.sendBefore(obsId, fileId)).sequence
        _   <- info(s"Start ${inst.resource.show} observation ${obsId.format} with label $fileId")
        r   <- inst.observe(config)(fileId)
        _   <- info(s"Completed ${inst.resource.show} observation ${obsId.format} with label $fileId")
        ret <- observeTail(fileId, d)(r)
      } yield ret

    def observeTail(id: ImageFileId, dataId: String)(r: ObserveCommandResult): F[Result[F]] = {
      def okTail(stopped: Boolean): F[Result[F]] = for {
        _ <- notifyObserveEnd
        _ <- headers(ctx).reverseMap(_.sendAfter(id)).sequence.void
        _ <- closeImage(id)
        _ <- sendDataEnd[F](systems, obsId, id, dataId)
      } yield if (stopped) Result.OKStopped(Response.Observed(id)) else Result.OK(Response.Observed(id))

      val successTail: F[Result[F]] = okTail(stopped = false)

      val stopTail: F[Result[F]] = okTail(stopped = true)

      r match {
        case ObserveCommandResult.Success => successTail
        case ObserveCommandResult.Stopped => stopTail
        case ObserveCommandResult.Aborted => abortTail(systems, obsId, id)
        case ObserveCommandResult.Paused  =>
          inst.calcObserveTime(config)
            .map(e => Result.Paused(ObserveContext(observeTail(id, dataId), e)))
      }
    }

    Stream.eval(dhsFileId(systems, inst).attempt).flatMap {
      case Right(id) =>
        val observationProgressStream =
          for {
            ot <- Stream.eval(inst.calcObserveTime(config))
            pr <- inst.observeProgress(ot, ElapsedTime(0.0.seconds))
          } yield Result.Partial(pr)

        val observationCommand =
          Stream.eval[F, Result[F]](doObserve(id))

        Stream.emit(Result.Partial(FileIdAllocated(id))) ++
          observationProgressStream.mergeHaltR(observationCommand)
      case Left(e: SeqexecFailure)   => Stream.emit(Result.Error(SeqexecFailure.explain(e)))
      case Left(e: Throwable)   => Stream.emit(Result.Error(SeqexecFailure.explain(SeqexecFailure.SeqexecException(e))))
    }
  }

}

object ObserveActions extends ObserveActions
