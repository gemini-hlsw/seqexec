// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi.client.gpi

import cats.Show
import cats.instances.string._
import cats.instances.double._
import cats.instances.int._
import cats.syntax.show._
import cats.effect.IO
import edu.gemini.aspen.giapi.commands.{Activity, Command, DefaultConfiguration, SequenceCommand}
import edu.gemini.giapi.client.{Giapi, commands}
import edu.gemini.giapi.client.commands.CommandResult
import fs2.Stream
import mouse.boolean._

/**
  * Client for GPI
  */
// Status items
class GPIClient[F[_]](giapi: Giapi[F]) {

  // Some items, more will be added as needed
  def heartbeat: F[Int] =
    giapi.get[Int]("gpi:heartbeat")

  def fpmMask: F[String] =
    giapi.get[String]("gpi:fpmMask")

  def aoDarkLevel: F[Float] =
    giapi.get[Float]("gpi:ao:darkLevel")

  // add more items...

  // Commands
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
      new Command(SequenceCommand.OBSERVE,
                  Activity.PRESET_START,
                  DefaultConfiguration
                    .configurationBuilder()
                    .withConfiguration(commands.DataLabelCfg, dataLabel.show)
                    .build()))

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
    giapi.command(new Command(SequenceCommand.APPLY, Activity.PRESET_START,
      DefaultConfiguration
        .configurationBuilder()
        .withConfiguration(s"gpi:selectShutter.$shutterName", position.fold("1", "0"))
        .build()))

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
    giapi.command(new Command(SequenceCommand.APPLY, Activity.PRESET_START,
      DefaultConfiguration
        .configurationBuilder()
        .withConfiguration("gpi:observationMode.mode", mode)
        .build()))

  def ifsFilter(filter: String): F[CommandResult] =
    giapi.command(new Command(SequenceCommand.APPLY, Activity.PRESET_START,
      DefaultConfiguration
        .configurationBuilder()
        .withConfiguration("gpi:ifs:selectIfsFilter.maskStr", filter)
        .build()))

  def ifsConfigure(integrationTime: Double, coAdds: Int, readoutMode: Int): F[CommandResult] =
    giapi.command(new Command(SequenceCommand.APPLY, Activity.PRESET_START,
      DefaultConfiguration
        .configurationBuilder()
        .withConfiguration("gpi:configIfs.integrationTime", integrationTime.show)
        .withConfiguration("gpi:configIfs.numCoadds", coAdds.show)
        .withConfiguration("gpi:configIfs.readoutMode", readoutMode.show)
        .build()))
}


object GPIExample extends App {
  private val gpiStatus =
    Stream.bracket(Giapi.giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)").connect)(
      giapi => {
        val client = new GPIClient[IO](giapi)
        val r =
          for {
            h <- client.heartbeat
            f <- client.fpmMask
            o <- client.aoDarkLevel
          } yield (h, f, o)
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  private val gpiSequence =
    Stream.bracket(Giapi.giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)").connect)(
      giapi => {
        val client = new GPIClient[IO](giapi)
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
