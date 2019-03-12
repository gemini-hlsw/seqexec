// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair
import cats.effect.IO
import cats.implicits._
import org.log4s.getLogger
import seqexec.server.altair.AltairController.FieldLens
import seqexec.server.tcs.Gaos.{PauseCondition, PauseResume, ResumeCondition}
import squants.Time

object AltairControllerSim extends AltairController[IO] {
  private val Log = getLogger

  override def pauseResume(pauseReasons: Set[PauseCondition], resumeReasons: Set[ResumeCondition], fieldLens: FieldLens)(cfg: AltairController.AltairConfig)
  : IO[PauseResume[IO]] = IO{ PauseResume(
    IO(Log.info(s"Simulate pausing Altair loops because of $pauseReasons")).some,
    IO(Log.info(s"Simulatet restoring Altair configuration $cfg because of $resumeReasons")).some
  ) }

  override def observe(expTime: Time)(cfg: AltairController.AltairConfig): IO[Unit] = IO{
    Log.info("Simulate observe notification for Altair")
  }

  override def endObserve(cfg: AltairController.AltairConfig): IO[Unit] = IO{
    Log.info("Simulate endObserve notification for Altair")
  }

  override def isFollowing: IO[Option[Boolean]] = IO(false.some)
}
