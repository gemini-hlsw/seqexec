// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair
import cats.effect.IO
import org.log4s.getLogger
import seqexec.server.altair.AltairController.FieldLens
import seqexec.server.tcs.Gaos
import squants.Time

object AltairControllerSim extends AltairController[IO] {
  private val Log = getLogger

  override def pause(reasons: Set[Gaos.PauseCondition], fieldLens: FieldLens)(cfg: AltairController.AltairConfig)
  : IO[Set[Gaos.ResumeCondition] => IO[Unit]] = IO{
    Log.info(s"Simulate pausing Altair loops because of $reasons")
    _:Set[Gaos.ResumeCondition] => IO{
      Log.info(s"Simulatet restoring Altair configuration $cfg because of $reasons")
    }
  }

  override def observe(expTime: Time)(cfg: AltairController.AltairConfig): IO[Unit] = IO{
    Log.info("Simulate observe notification for Altair")
  }

  override def endObserve(cfg: AltairController.AltairConfig): IO[Unit] = IO{
    Log.info("Simulate endObserve notification for Altair")
  }
}
