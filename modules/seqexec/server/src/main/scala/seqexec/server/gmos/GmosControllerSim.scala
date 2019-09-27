// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats._
import cats.implicits._
import cats.effect.Sync
import cats.effect.Timer
import cats.effect.concurrent.Ref
import io.chrisdavenport.log4cats.Logger
import monocle.Optional
import monocle.macros.Lenses
import monocle.std.option.some
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.InstrumentSystem.ElapsedTime
import seqexec.server.gmos.GmosController.GmosConfig
import seqexec.server.gmos.GmosController.NorthTypes
import seqexec.server.gmos.GmosController.SiteDependentTypes
import seqexec.server.gmos.GmosController.SouthTypes
import seqexec.server.gmos.GmosController.Config.NSConfig
import seqexec.server.InstrumentControllerSim
import seqexec.server.Progress
import squants.Time
import scala.concurrent.duration._

/**
  * Keep track of the current execution state
  */
@Lenses
final case class NSCurrent(
  fileId:        ImageFileId,
  totalCycles:   Int,
  exposureCount: Int,
  expTime:       Time
) {
  def lastSubexposure: Boolean =
    (exposureCount + 1) === totalCycles * Gmos.NsSequence.length

  val cycle = exposureCount / Gmos.NsSequence.length
}

object NSCurrent {
  implicit val showNSCurrent: Show[NSCurrent] = Show.show { a =>
    s"NS State: file=${a.fileId}, cycle=${a.cycle + 1}, stage=${Gmos.NsSequence.toList
      .lift(a.exposureCount % Gmos.NsSequence.length)
      .getOrElse("Unknown")}/${(a.exposureCount % Gmos.NsSequence.length) + 1}, subexposure=${a.exposureCount + 1}, expTime=${a.expTime}"
  }
}

/**
  * Used to keep the internal state of NS
  */
@Lenses
final case class NSObsState(config: NSConfig, current: Option[NSCurrent])

object NSObsState {
  def fromConfig(c: NSConfig): NSObsState =
    NSObsState(c, None)

  val Zero: NSObsState = NSObsState(NSConfig.NoNodAndShuffle, None)

  val fileId: Optional[NSObsState, ImageFileId] =
    NSObsState.current ^<-? some ^|-> NSCurrent.fileId

  val exposureCount: Optional[NSObsState, Int] =
    NSObsState.current ^<-? some ^|-> NSCurrent.exposureCount

  val expTime: Optional[NSObsState, Time] =
    NSObsState.current ^<-? some ^|-> NSCurrent.expTime

}

object GmosControllerSim {
  def apply[F[_]: FlatMap: Timer, T <: SiteDependentTypes](
    sim:      InstrumentControllerSim[F],
    nsConfig: Ref[F, NSObsState]
  ): GmosController[F, T] =
    new GmosController[F, T] {
      override def observe(
        fileId:  ImageFileId,
        expTime: Time
      ): F[ObserveCommandResult] =
        nsConfig.modify {
          case s @ NSObsState(NSConfig.NoNodAndShuffle, _) =>
            (s, s)
          case s @ NSObsState(NSConfig.NodAndShuffle(cycles, _, _), _) =>
            // Initialize the current state
            val update =
              NSObsState.current.set(NSCurrent(fileId, cycles, 0, expTime).some)
            (update(s), update(s))
        } >>= {
          case NSObsState(NSConfig.NodAndShuffle(_, _, _), Some(curr)) =>
            sim.log(s"Simulate Gmos N&S observation ${curr.show}") *>
              // Initial N&S obs
              sim.observe(fileId, expTime).as(ObserveCommandResult.Partial)
          case NSObsState(_, _) =>
            sim.observe(fileId, expTime) // Regular observation
        }

      override def applyConfig(config: GmosConfig[T]): F[Unit] =
        nsConfig.set(NSObsState.fromConfig(config.ns)) *> // Keep the state of NS Config
          sim.applyConfig(config)

      override def setRowsToShuffle(rows: Int): F[Unit] =
        sim.log(s"Set rows to shuffle to $rows") *>
          Timer[F].sleep(new FiniteDuration(2, SECONDS))

      override def stopObserve: F[Unit] = sim.stopObserve

      override def abortObserve: F[Unit] = sim.abortObserve

      override def endObserve: F[Unit] = sim.endObserve

      override def pauseObserve: F[Unit] = sim.pauseObserve

      override def resumePaused(expTime: Time): F[ObserveCommandResult] =
        nsConfig.modify {
          case s @ NSObsState(NSConfig.NodAndShuffle(_, _, _), Some(curr))
              if !curr.lastSubexposure =>
            // We should keep track of where on a N&S Sequence are we
            // Let's just increase the exposure counter
            val upd = NSObsState.exposureCount.modify(_ + 1)
            (upd(s), upd(s))
          case s =>
            (s, s)
        } >>= {
          case NSObsState(NSConfig.NodAndShuffle(_, _, _), Some(curr))
              if !curr.lastSubexposure =>
            sim.log(s"Next Nod ${curr.show}") *>
              sim.observe(curr.fileId, expTime).as(ObserveCommandResult.Partial)
          case NSObsState(NSConfig.NodAndShuffle(_, _, _), Some(curr))
              if curr.lastSubexposure =>
            sim.log(s"Final Nod ${curr.show}") *>
              sim.observe(curr.fileId, expTime)
          case _ =>
            // Regular observation
            sim.resumePaused
        }

      override def stopPaused: F[ObserveCommandResult] = sim.stopPaused

      override def abortPaused: F[ObserveCommandResult] = sim.abortPaused

      override def observeProgress(
        total:   Time,
        elapsed: ElapsedTime
      ): fs2.Stream[F, Progress] =
        sim.observeCountdown(total, elapsed)

    }

  def unsafeSouth[F[_]: Sync: Logger: Timer]: GmosController[F, SouthTypes] = {
    val nsConfig: Ref[F, NSObsState] = Ref.unsafe(NSObsState.Zero)
    val sim: InstrumentControllerSim[F] = InstrumentControllerSim.unsafeApply[F](s"GMOS South")
    GmosControllerSim[F, SouthTypes](sim, nsConfig)
  }

  def unsafeNorth[F[_]: Sync: Logger: Timer]: GmosController[F, NorthTypes] = {
    val nsConfig: Ref[F, NSObsState] = Ref.unsafe(NSObsState.Zero)
    val sim: InstrumentControllerSim[F] = InstrumentControllerSim.unsafeApply[F](s"GMOS North")
    GmosControllerSim[F, NorthTypes](sim, nsConfig)
  }
}
