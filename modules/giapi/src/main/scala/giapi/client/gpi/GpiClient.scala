// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.gpi

import cats.implicits._
import edu.gemini.aspen.giapi.commands.{Activity, SequenceCommand}
import fs2.Stream
import giapi.client.commands.{Command, CommandResult, Configuration}
import giapi.client.{Giapi, GiapiClient}
import giapi.client.syntax.giapiconfig._
import mouse.boolean._

/**
  * Client for GPI
  */
final class GpiClient[F[_]](override val giapi: Giapi[F]) extends GiapiClient[F] {
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
}

object GPIExample extends cats.effect.IOApp {

  import cats.effect.{ IO, Resource, ExitCode }
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext

  val connect: Resource[IO, GpiClient[IO]] =
    Resource.make(
      Giapi.giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)", ExecutionContext.global)
           .connect
      )(_.close).map(new GpiClient[IO](_))

  val gpiStatus: IO[(Vector[Int], Int, String, Float)] =
    connect.use { client =>
      for {
        hs <- client.heartbeatS.flatMap(_.take(3).compile.toVector)
        h  <- client.heartbeat
        f  <- client.fpmMask
        o  <- client.aoDarkLevel
      } yield (hs, h, f, o)
    }

  val gpiSequence: IO[CommandResult] =
    connect.use { client =>
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

