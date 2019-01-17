// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.gpi

import cats.implicits._
import cats.effect.IO
import cats.effect.Sync
import cats.effect.Resource
import cats.effect.ConcurrentEffect
import edu.gemini.aspen.giapi.commands.Activity
import edu.gemini.aspen.giapi.commands.SequenceCommand
import fs2.Stream
import giapi.client.commands.Command
import giapi.client.commands.CommandResult
import giapi.client.commands.Configuration
import giapi.client.Giapi
import giapi.client.GiapiClient
import giapi.client.GiapiStatusDb
import giapi.client.syntax.giapiconfig._
import mouse.boolean._
import scala.concurrent.ExecutionContext

/**
  * Client for GPI
  */
final class GpiClient[F[_]: Sync] private (override val giapi: Giapi[F],
                                           statusDb:           GiapiStatusDb[F])
    extends GiapiClient[F] {
  import GiapiClient.DefaultCommandTimeout

  ///////////////
  // Status items
  ///////////////
  def heartbeat: F[Int] =
    giapi.get[Int]("gpi:heartbeat")

  def fpmMask: F[String] =
    giapi.get[String]("gpi:fpmMask")

  def aoDarkLevel: F[Float] =
    giapi.get[Float]("gpi:ao:darkLevel")

  /////////////////////
  // Streaming statuses
  /////////////////////
  def heartbeatS: F[Stream[F, Int]] =
    giapi.stream[Int]("gpi:heartbeat")

  ////////////////////////
  // GPI Specific commands
  ////////////////////////

  // TODO Use OCS constants for open/close
  private def shutter(shutterName: String,
                      position: Boolean): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        Configuration.single(s"gpi:selectShutter.$shutterName",
                             position.fold(1, 0))
      ), DefaultCommandTimeout)

  def entranceShutter(position: Boolean): F[CommandResult] =
    shutter("entranceShutter", position)

  def calExitShutter(position: Boolean): F[CommandResult] =
    shutter("calExitShutter", position)

  def calEntranceShutter(position: Boolean): F[CommandResult] =
    shutter("calEntranceShutter", position)

  def calReferenceShutter(position: Boolean): F[CommandResult] =
    shutter("calReferenceShutter", position)

  def calScienceShutter(position: Boolean): F[CommandResult] =
    shutter("calScienceShutter", position)

  // TODO Use OCS constants
  def observingMode(mode: String): F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.APPLY,
              Activity.PRESET_START,
              Configuration.single("gpi:observationMode.mode", mode)),
      DefaultCommandTimeout)

  def ifsFilter(filter: String): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        Configuration.single("gpi:ifs:selectIfsFilter.maskStr", filter)
      ),
      DefaultCommandTimeout)

  def ifsConfigure(integrationTime: Double,
                   coAdds: Int,
                   readoutMode: Int): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        List(
          Configuration.single("gpi:configIfs.integrationTime",
                               integrationTime),
          Configuration.single("gpi:configIfs.numCoadds", coAdds),
          Configuration.single("gpi:configIfs.readoutMode", readoutMode)
        ).combineAll
      ),
      DefaultCommandTimeout
    )

  override def genericApply(configuration: Configuration): F[CommandResult] = {
    // TODO Implement a smarter apply
    def smartApply(): F[CommandResult] =
      giapi.command(Command(
                      SequenceCommand.APPLY,
                      Activity.PRESET_START,
                      configuration
                    ),
                    DefaultCommandTimeout)

    for {
      _ <- statusDb.value("gpi:fpu") // placeholder
      a <- smartApply()
    } yield a
  }
}

object GpiClient {
  // Used for simulations
  def simulatedGpiClient(ec: ExecutionContext): Resource[IO, GpiClient[IO]] =
    Resource.liftF(
      for {
        c <- Giapi.giapiConnectionIO(ec).connect
      } yield new GpiClient(c, GiapiStatusDb.simulatedDb[IO])
    )

  def gpiClient[F[_]: ConcurrentEffect](
    url:     String,
    context: ExecutionContext): Resource[F, GpiClient[F]] = {
    val giapi: Resource[F, Giapi[F]] =
      Resource.make(
        Giapi.giapiConnection[F](url, context).connect
      )(_.close)

    val db: Resource[F, GiapiStatusDb[F]] =
      Resource.make(
        GiapiStatusDb.newStatusDb[F](url, List("gpi:heartbeat"))
      )(_.close)

    for {
      c <- giapi
      d <- db
    } yield new GpiClient[F](c, d)
  }

}

object GPIExample extends cats.effect.IOApp {

  import cats.effect.IO
  import cats.effect.ExitCode
  import scala.concurrent.duration._

  val url = "failover:(tcp://127.0.0.1:61616)"

  val gpi: Resource[IO, GpiClient[IO]] =
    GpiClient.gpiClient[IO](url, ExecutionContext.global)

  val gpiStatus: IO[(Vector[Int], Int, String, Float)] =
    gpi.use { client =>
      for {
        hs <- client.heartbeatS.flatMap(_.take(3).compile.toVector)
        h  <- client.heartbeat
        f  <- client.fpmMask
        o  <- client.aoDarkLevel
      } yield (hs, h, f, o)
    }

  val gpiSequence: IO[CommandResult] =
    gpi.use { client =>
      for {
        _ <- client.calExitShutter(true) // Open the shutter
        _ <- client.observingMode("Y_coron") // Change observing mode
        _ <- client.ifsConfigure(1.5, 1, 4) // Configure the IFS
        f <- client.observe("TEST_S20180509", 5.seconds) // observe
        _ <- client.park // Park at the end
      } yield f
    }

  def putLn(a: Any): IO[Unit] =
    IO.delay(println(a)) // scalastyle:off console.io

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- gpiStatus.flatMap(putLn)
      _ <- gpiSequence.flatMap(putLn)
    } yield ExitCode.Success

}
