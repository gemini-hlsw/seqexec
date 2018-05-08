// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.giapi.client.gpi

import cats.effect.IO
import edu.gemini.aspen.giapi.commands.{Activity, Command, SequenceCommand}
import edu.gemini.giapi.client.Giapi
import edu.gemini.giapi.client.commands.CommandResult
import fs2.Stream

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
    giapi.command(new Command(SequenceCommand.TEST, Activity.START))

  def init: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.INIT, Activity.START))

  def datum: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.DATUM, Activity.START))

  def park: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.PARK, Activity.START))

  def verify: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.VERIFY, Activity.START))

  def endVerify: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.END_VERIFY, Activity.START))

  def guide: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.GUIDE, Activity.START))

  def endGuide: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.END_GUIDE, Activity.START))

  def endObserve: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.END_OBSERVE, Activity.START))

  def pause: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.PAUSE, Activity.START))

  def continue: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.CONTINUE, Activity.START))

  def stop: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.STOP, Activity.START))

  def abort: F[CommandResult] =
    giapi.command(new Command(SequenceCommand.ABORT, Activity.START))
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

  private val gpiPark =
    Stream.bracket(Giapi.giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)").connect)(
      giapi => {
        val client = new GPIClient[IO](giapi)
        val r =
          for {
            c <- client.park
          } yield c
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  (gpiStatus ++ gpiPark).compile.drain.unsafeRunSync()
}
