// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.gpi

import cats.Show
import cats.implicits._
import edu.gemini.aspen.giapi.commands.{Activity, SequenceCommand}
import fs2.Stream
import giapi.client.commands.{Command, Configuration, CommandResult}
import giapi.client.{Giapi, commands}
import mouse.boolean._
import scala.concurrent.ExecutionContext

/**
  * Client for GPI
  */
class GPIClient[F[_]](giapi: Giapi[F], ec: ExecutionContext) {

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
    giapi.stream[Int]("gpi:heartbeat", ec)

  ///////////////////
  // General commands
  ///////////////////
  def test: F[CommandResult] =
    giapi.command(Command(SequenceCommand.TEST, Activity.PRESET_START, Configuration.Zero))

  def init: F[CommandResult] =
    giapi.command(Command(SequenceCommand.INIT, Activity.PRESET_START, Configuration.Zero))

  def datum: F[CommandResult] =
    giapi.command(Command(SequenceCommand.DATUM, Activity.PRESET_START, Configuration.Zero))

  def park: F[CommandResult] =
    giapi.command(Command(SequenceCommand.PARK, Activity.PRESET_START, Configuration.Zero))

  def verify: F[CommandResult] =
    giapi.command(Command(SequenceCommand.VERIFY, Activity.PRESET_START, Configuration.Zero))

  def endVerify: F[CommandResult] =
    giapi.command(Command(SequenceCommand.END_VERIFY, Activity.PRESET_START, Configuration.Zero))

  def guide: F[CommandResult] =
    giapi.command(Command(SequenceCommand.GUIDE, Activity.PRESET_START, Configuration.Zero))

  def endGuide: F[CommandResult] =
    giapi.command(Command(SequenceCommand.END_GUIDE, Activity.PRESET_START, Configuration.Zero))

  def observe[A: Show](dataLabel: A): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.OBSERVE,
        Activity.PRESET_START,
        Configuration.single(commands.DataLabelCfg, dataLabel)
      ))

  def endObserve: F[CommandResult] =
    giapi.command(Command(SequenceCommand.END_OBSERVE, Activity.PRESET_START, Configuration.Zero))

  def pause: F[CommandResult] =
    giapi.command(Command(SequenceCommand.PAUSE, Activity.PRESET_START, Configuration.Zero))

  def continue: F[CommandResult] =
    giapi.command(Command(SequenceCommand.CONTINUE, Activity.PRESET_START, Configuration.Zero))

  def stop: F[CommandResult] =
    giapi.command(Command(SequenceCommand.STOP, Activity.PRESET_START, Configuration.Zero))

  def abort: F[CommandResult] =
    giapi.command(Command(SequenceCommand.ABORT, Activity.PRESET_START, Configuration.Zero))

  ////////////////////////
  // GPI Specific commands
  ////////////////////////

  // TODO Use OCS constants for open/close
  private def shutter(shutterName: String, position: Boolean): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        Configuration.single(s"gpi:selectShutter.$shutterName", position.fold(1, 0))
      ))

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
                  Configuration.single("gpi:observationMode.mode", mode)
      ))

  def ifsFilter(filter: String): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        Configuration.single("gpi:ifs:selectIfsFilter.maskStr", filter)
      ))

  def ifsConfigure(integrationTime: Double, coAdds: Int, readoutMode: Int): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        List(Configuration.single("gpi:configIfs.integrationTime", integrationTime),
          Configuration.single("gpi:configIfs.numCoadds", coAdds),
          Configuration.single("gpi:configIfs.readoutMode", readoutMode)).combineAll
      ))

  def genericApply(configuration: Configuration): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        configuration
      ))
}

object GPIExample extends App {

  import cats.effect.IO
  import scala.concurrent.duration._

  private val gpiStatus =
    Stream.bracket(
      Giapi.giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)", 2000.millis).connect)(
      giapi => {
        val client = new GPIClient[IO](giapi, scala.concurrent.ExecutionContext.Implicits.global)
        val r =
          for {
            hs <- client.heartbeatS.flatMap(_.take(3).compile.toVector)
            h  <- client.heartbeat
            f  <- client.fpmMask
            o  <- client.aoDarkLevel
          } yield (hs, h, f, o)
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  private val gpiSequence =
    Stream.bracket(
      Giapi.giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)", 2000.millis).connect)(
      giapi => {
        val client = new GPIClient[IO](giapi, scala.concurrent.ExecutionContext.Implicits.global)
        val r =
          for {
            _ <- client.calExitShutter(true) // Open the shutter
            - <- client.observingMode("Y_coron") // Change observing mode
            _ <- client.ifsConfigure(1.5, 1, 4) // Configure the IFS
            f <- client.observe("TEST_S20180509") // observe
            _ <- client.park // Park at the end
          } yield f
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  (gpiStatus ++ gpiSequence).compile.drain.unsafeRunSync()
}
