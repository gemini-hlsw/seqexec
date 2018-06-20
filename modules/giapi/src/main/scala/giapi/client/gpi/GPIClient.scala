// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.gpi

import cats.Show
import cats.instances.double._
import cats.instances.int._
import cats.syntax.show._
import edu.gemini.aspen.giapi.commands.{Activity, Command, Configuration, DefaultConfiguration, SequenceCommand}
import fs2.Stream
import giapi.client.commands.CommandResult
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
    giapi.command(new Command(SequenceCommand.TEST, Activity.PRESET_START))

  def init: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.INIT, Activity.PRESET_START))

  def datum: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.DATUM, Activity.PRESET_START))

  def park: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.PARK, Activity.PRESET_START))

  def verify: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.VERIFY, Activity.PRESET_START))

  def endVerify: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.END_VERIFY, Activity.PRESET_START))

  def guide: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.GUIDE, Activity.PRESET_START))

  def endGuide: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.END_GUIDE, Activity.PRESET_START))

  def observe[A: Show](dataLabel: A): F[CommandResult] =
    giapi.command(
      new Command(
        SequenceCommand.OBSERVE,
        Activity.PRESET_START,
        DefaultConfiguration
          .configurationBuilder()
          .withConfiguration(commands.DataLabelCfg, dataLabel.show)
          .build()
      ))

  def endObserve: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.END_OBSERVE, Activity.PRESET_START))

  def pause: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.PAUSE, Activity.PRESET_START))

  def continue: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.CONTINUE, Activity.PRESET_START))

  def stop: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.STOP, Activity.PRESET_START))

  def abort: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.ABORT, Activity.PRESET_START))

  ////////////////////////
  // GPI Specific commands
  ////////////////////////

  // TODO Use OCS constants for open/close
  private def shutter(shutterName: String, position: Boolean): F[CommandResult] =
    giapi.command(
      new Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        DefaultConfiguration
          .configurationBuilder()
          .withConfiguration(s"gpi:selectShutter.$shutterName", position.fold("1", "0"))
          .build()
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
      new Command(SequenceCommand.APPLY,
                  Activity.PRESET_START,
                  DefaultConfiguration
                    .configurationBuilder()
                    .withConfiguration("gpi:observationMode.mode", mode)
                    .build()))

  def ifsFilter(filter: String): F[CommandResult] =
    giapi.command(
      new Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        DefaultConfiguration
          .configurationBuilder()
          .withConfiguration("gpi:ifs:selectIfsFilter.maskStr", filter)
          .build()
      ))

  def ifsConfigure(integrationTime: Double, coAdds: Int, readoutMode: Int): F[CommandResult] =
    giapi.command(
      new Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        DefaultConfiguration
          .configurationBuilder()
          .withConfiguration("gpi:configIfs.integrationTime", integrationTime.show)
          .withConfiguration("gpi:configIfs.numCoadds", coAdds.show)
          .withConfiguration("gpi:configIfs.readoutMode", readoutMode.show)
          .build()
      ))

  def genericApply(configuration: Configuration): F[CommandResult] =
    giapi.command(
      new Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        configuration
      ))
}

object GPIExample extends App {

  import cats.effect.IO
  import cats.instances.string._

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
