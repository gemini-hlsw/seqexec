// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import scala.concurrent.duration._

import cats._
import cats.effect._
import cats.syntax.all._
import fs2.Stream
import org.typelevel.log4cats.Logger
import seqexec.engine._
import seqexec.model.Observation
import seqexec.model.dhs._
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.InstrumentSystem._
import squants.time.Time
import squants.time.TimeConversions._

/**
 * Methods usedd to generate observation related actions
 */
trait ObserveActions {

  private def info[F[_]: Logger](msg: => String): F[Unit] = Logger[F].info(msg)

  /**
   * Actions to perform when an observe is aborted
   */
  def abortTail[F[_]: MonadError[*[_], Throwable]](
    odb:         OdbProxy[F],
    obsId:       Observation.Id,
    imageFileId: ImageFileId
  ): F[Result[F]] =
    odb
      .obsAbort(obsId, imageFileId)
      .ensure(
        SeqexecFailure
          .Unexpected("Unable to send ObservationAborted message to ODB.")
      )(identity)
      .as(Result.OKAborted(Response.Aborted(imageFileId)))

  /**
   * Send the datasetStart command to the odb
   */
  private def sendDataStart[F[_]: MonadError[*[_], Throwable]](
    odb:         OdbProxy[F],
    obsId:       Observation.Id,
    imageFileId: ImageFileId,
    dataId:      DataId
  ): F[Unit] =
    odb
      .datasetStart(obsId, dataId, imageFileId)
      .ensure(
        SeqexecFailure.Unexpected("Unable to send DataStart message to ODB.")
      )(identity)
      .void

  /**
   * Send the datasetEnd command to the odb
   */
  private def sendDataEnd[F[_]: MonadError[*[_], Throwable]](
    odb:         OdbProxy[F],
    obsId:       Observation.Id,
    imageFileId: ImageFileId,
    dataId:      DataId
  ): F[Unit] =
    odb
      .datasetComplete(obsId, dataId, imageFileId)
      .ensure(
        SeqexecFailure.Unexpected("Unable to send DataEnd message to ODB.")
      )(identity)
      .void

  /**
   * Standard progress stream for an observation
   */
  def observationProgressStream[F[_]](
    env: ObserveEnvironment[F]
  ): Stream[F, Result[F]] =
    for {
      ot <- Stream.eval(env.inst.calcObserveTime(env.config))
      pr <- env.inst.observeProgress(ot, ElapsedTime(0.0.seconds))
    } yield Result.Partial(pr)

  /**
   * Tell each subsystem that an observe will start
   */
  def notifyObserveStart[F[_]: Applicative](
    env: ObserveEnvironment[F]
  ): F[Unit] =
    env.otherSys.traverse_(_.notifyObserveStart)

  /**
   * Tell each subsystem that an observe will end
   * Unlike observe start we also tell the instrumetn about it
   */
  def notifyObserveEnd[F[_]: Applicative](env: ObserveEnvironment[F]): F[Unit] =
    (env.inst +: env.otherSys).traverse_(_.notifyObserveEnd)

  /**
   * Close the image, telling either DHS or GDS as it correspond
   */
  def closeImage[F[_]](id: ImageFileId, env: ObserveEnvironment[F]): F[Unit] =
    env.inst.keywordsClient.closeImage(id)

  /**
   * Preamble for observations. It tells the odb, the subsystems
   * send the start headers and finally sends an observe
   */
  def observePreamble[F[_]: Concurrent: Timer: Logger](
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): F[ObserveCommandResult] =
    for {
      _ <- sendDataStart(env.odb, env.obsId, fileId, env.dataId)
      _ <- notifyObserveStart(env)
      _ <- env.headers(env.ctx).traverse(_.sendBefore(env.obsId, fileId))
      _ <- info(
             s"Start ${env.inst.resource.show} observation ${env.obsId.format} with label $fileId"
           )
      r <- env.inst.observe(env.config)(fileId)
      _ <-
        info(
          s"Completed ${env.inst.resource.show} observation ${env.obsId.format} with label $fileId"
        )
    } yield r

  /**
   * End of an observation for a typical instrument
   * It tells the odb and each subsystem and also sends the end
   * observation keywords
   */
  def okTail[F[_]: Concurrent: Timer](
    fileId:  ImageFileId,
    stopped: Boolean,
    env:     ObserveEnvironment[F]
  ): F[Result[F]] =
    for {
      _ <- notifyObserveEnd(env)
      _ <- env.headers(env.ctx).reverseIterator.toList.traverse(_.sendAfter(fileId))
      _ <- closeImage(fileId, env)
      _ <- sendDataEnd(env.odb, env.obsId, fileId, env.dataId)
    } yield
      if (stopped) Result.OKStopped(Response.Observed(fileId))
      else Result.OK(Response.Observed(fileId))

  /**
   * Method to process observe results and act accordingly to the response
   */
  private def observeTail[F[_]: Timer](
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  )(r:      ObserveCommandResult)(implicit cio: Concurrent[F]): Stream[F, Result[F]] =
    Stream.eval(r match {
      case ObserveCommandResult.Success =>
        okTail(fileId, stopped = false, env)
      case ObserveCommandResult.Stopped =>
        okTail(fileId, stopped = true, env)
      case ObserveCommandResult.Aborted =>
        abortTail(env.odb, env.obsId, fileId)
      case ObserveCommandResult.Paused  =>
        env.inst
          .calcObserveTime(env.config)
          .flatMap(totalTime =>
            env.inst.observeControl(env.config) match {
              case c: CompleteControl[F] =>
                val resumePaused: Time => Stream[F, Result[F]]    =
                  (remaining: Time) =>
                    Stream
                      .eval {
                        c.continue
                          .self(remaining)
                      }
                      .flatMap(observeTail(fileId, env))
                val progress: ElapsedTime => Stream[F, Result[F]] =
                  (elapsed: ElapsedTime) =>
                    env.inst
                      .observeProgress(totalTime, elapsed)
                      .map(Result.Partial(_))
                      .widen[Result[F]]
                val stopPaused: Stream[F, Result[F]]              =
                  Stream
                    .eval {
                      c.stopPaused.self
                    }
                    .flatMap(observeTail(fileId, env))
                val abortPaused: Stream[F, Result[F]]             =
                  Stream
                    .eval {
                      c.abortPaused.self
                    }
                    .flatMap(observeTail(fileId, env))

                Result
                  .Paused(
                    ObserveContext[F](
                      resumePaused,
                      progress,
                      stopPaused,
                      abortPaused,
                      totalTime
                    )
                  )
                  .pure[F]
                  .widen[Result[F]]
              case _                     =>
                SeqexecFailure
                  .Execution("Observation paused for an instrument that does not support pause")
                  .raiseError[F, Result[F]]
            }
          )
    })

  /**
   * Observe for a typical instrument
   */
  def stdObserve[F[_]: Concurrent: Timer: Logger](
    fileId: ImageFileId,
    env:    ObserveEnvironment[F]
  ): Stream[F, Result[F]] =
    for {
      result <- Stream.eval(observePreamble(fileId, env))
      ret    <- observeTail(fileId, env)(result)
    } yield ret

}

object ObserveActions extends ObserveActions
