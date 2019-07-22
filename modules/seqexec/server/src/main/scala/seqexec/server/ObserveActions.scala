// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.effect.Sync
import cats.effect.Concurrent
import cats.data.Reader
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
import seqexec.server.SeqTranslate._
import squants.time.TimeConversions._

/**
 * Methods usedd to generate observation related actions
 */
trait ObserveActions {
  private val Log = getLogger

  // All instruments ask the DHS for an ImageFileId
  private def dhsFileId[F[_]: ApplicativeError[?[_], Throwable]](systems: Systems[F], inst: InstrumentSystem[F]): SeqActionF[F, ImageFileId] =
    SeqActionF.embedF(systems.dhs.createImage(DhsClient.ImageParameters(DhsClient.Permanent, List(inst.contributorName, "dhs-http"))))

  private def info[F[_]: Sync](msg: => String): SeqActionF[F, Unit] = SeqActionF.liftF(Sync[F].delay(Log.info(msg)))

  private def sendObservationAborted[F[_]: Monad](systems: Systems[F], obsId: Observation.Id, imageFileId: ImageFileId): SeqActionF[F, Unit] =
    systems.odb.obsAbort(obsId, imageFileId).ifM(
      SeqActionF.void,
      SeqActionF.raiseException(SeqexecFailure.Unexpected("Unable to send ObservationAborted message to ODB.")))

  private def sendDataStart[F[_]: Monad](systems: Systems[F], obsId: Observation.Id, imageFileId: ImageFileId, dataId: String): SeqActionF[F, Unit] =
    systems.odb.datasetStart(obsId, dataId, imageFileId).ifM(
      SeqActionF.void,
      SeqActionF.raiseException(SeqexecFailure.Unexpected("Unable to send DataStart message to ODB."))
    )

  private def sendDataEnd[F[_]: Monad](systems: Systems[F], obsId: Observation.Id, imageFileId: ImageFileId, dataId: String): SeqActionF[F, Unit] =
    systems.odb.datasetComplete(obsId, dataId, imageFileId).ifM(
      SeqActionF.void,
      SeqActionF.raiseException(SeqexecFailure.Unexpected("Unable to send DataEnd message to ODB.")))

  def observe[F[_]: Sync](systems: Systems[F], config: Config, obsId: Observation.Id, inst: InstrumentSystem[F],
                      otherSys: List[System[F]], headers: Reader[HeaderExtraData, List[Header[F]]])
                     (ctx: HeaderExtraData)
                     (implicit ev: Concurrent[F]): Stream[F, Result[F]]
  = {
    def dataId: SeqActionF[F, String] = SeqActionF.either(
      config.extractAs[String](OBSERVE_KEY / DATA_LABEL_PROP).leftMap(e =>
      SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))))

    def notifyObserveStart: SeqActionF[F, Unit] =
      otherSys.map(_.notifyObserveStart).sequence.void

    // endObserve must be sent to the instrument too.
    def notifyObserveEnd: SeqActionF[F, Unit] =
      (inst +: otherSys).map(_.notifyObserveEnd).sequence.void

    def closeImage(id: ImageFileId): SeqActionF[F, Unit] =
      SeqActionF.embedF(inst.keywordsClient.closeImage(id))

    def doObserve(fileId: ImageFileId): SeqActionF[F, Result[F]] =
      for {
        d   <- dataId
        _   <- sendDataStart(systems, obsId, fileId, d)
        _   <- notifyObserveStart
        _   <- SeqActionF.embedF(headers(ctx).map(_.sendBefore(obsId, fileId)).sequence)
        _   <- info(s"Start ${inst.resource.show} observation ${obsId.format} with label $fileId")
        r   <- inst.observe(config)(fileId)
        _   <- info(s"Completed ${inst.resource.show} observation ${obsId.format} with label $fileId")
        ret <- observeTail(fileId, d)(r)
      } yield ret

    def observeTail(id: ImageFileId, dataId: String)(r: ObserveCommandResult): SeqActionF[F, Result[F]] = {
      def okTail(stopped: Boolean): SeqActionF[F, Result[F]] = for {
        _ <- notifyObserveEnd
        _ <- SeqActionF.embedF(headers(ctx).reverseMap(_.sendAfter(id)).sequence.void)
        _ <- closeImage(id)
        _ <- sendDataEnd[F](systems, obsId, id, dataId)
      } yield if (stopped) Result.OKStopped(Response.Observed(id)) else Result.OK(Response.Observed(id))

      val successTail: SeqActionF[F, Result[F]] = okTail(stopped = false)

      val stopTail: SeqActionF[F, Result[F]] = okTail(stopped = true)

      val abortTail: SeqActionF[F, Result[F]] = sendObservationAborted(systems, obsId, id) *>
        SeqActionF.raiseException(SeqexecFailure.Execution(s"Observation ${obsId.format} aborted by user."))

      r match {
        case ObserveCommandResult.Success => successTail
        case ObserveCommandResult.Stopped => stopTail
        case ObserveCommandResult.Aborted => abortTail
        case ObserveCommandResult.Paused  =>
          SeqActionF.liftF(inst.calcObserveTime(config))
            .map(e => Result.Paused(ObserveContext(observeTail(id, dataId), e)))
      }
    }

    Stream.eval(dhsFileId(systems, inst).value).flatMap {
      case Right(id) =>
        val observationProgressStream =
          for {
            ot <- Stream.eval(inst.calcObserveTime(config))
            pr <- inst.observeProgress(ot, ElapsedTime(0.0.seconds))
          } yield Result.Partial(pr)

        val observationCommand =
          Stream.eval[F, Result[F]](doObserve(id).value.map(_.toResult))

        Stream.emit(Result.Partial(FileIdAllocated(id))) ++
          observationProgressStream.mergeHaltR(observationCommand)
      case Left(e)   => Stream.emit(Result.Error(SeqexecFailure.explain(e)))
    }
  }

}

object ObserveActions extends ObserveActions
