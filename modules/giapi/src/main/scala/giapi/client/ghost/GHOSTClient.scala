package giapi.client.ghost

import cats.Show
import cats.implicits._
import edu.gemini.aspen.giapi.commands.{Activity, SequenceCommand}
import fs2.Stream
import giapi.client.commands.{Command, CommandResult, Configuration}
import giapi.client.{Giapi, commands}
import scala.concurrent.duration._

/**
  * Client for GHOST
  */
class GHOSTClient[F[_]](giapi: Giapi[F]) {
  // TODO: Find out the timeout for GHOST. Placeholder for now.
  val DefaultCommandTimeout: FiniteDuration = 60.seconds

  ///////////////////
  // General commands
  ///////////////////
  def test: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.TEST, Activity.PRESET_START, Configuration.Zero),
      DefaultCommandTimeout)

  def init: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.INIT, Activity.PRESET_START, Configuration.Zero),
      DefaultCommandTimeout)

  def datum: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.DATUM, Activity.PRESET_START, Configuration.Zero),
      DefaultCommandTimeout)

  def park: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.PARK, Activity.PRESET_START, Configuration.Zero),
      DefaultCommandTimeout)

  def verify: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.VERIFY,
        Activity.PRESET_START,
        Configuration.Zero), DefaultCommandTimeout)

  def endVerify: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.END_VERIFY,
        Activity.PRESET_START,
        Configuration.Zero), DefaultCommandTimeout)

  def guide: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.GUIDE, Activity.PRESET_START, Configuration.Zero),
      DefaultCommandTimeout)

  def endGuide: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.END_GUIDE,
        Activity.PRESET_START,
        Configuration.Zero), DefaultCommandTimeout)

  def observe[A: Show](dataLabel: A, expTime: FiniteDuration): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.OBSERVE,
        Activity.PRESET_START,
        Configuration.single(commands.DataLabelCfg, dataLabel)
      ), expTime)

  def endObserve: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.END_OBSERVE,
        Activity.PRESET_START,
        Configuration.Zero), DefaultCommandTimeout)

  def pause: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.PAUSE, Activity.PRESET_START, Configuration.Zero),
      DefaultCommandTimeout)

  def continue: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.CONTINUE,
        Activity.PRESET_START,
        Configuration.Zero), DefaultCommandTimeout)

  def stop: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.STOP, Activity.PRESET_START, Configuration.Zero),
      DefaultCommandTimeout)

  def abort: F[CommandResult] =
    giapi.command(
      Command(SequenceCommand.ABORT, Activity.PRESET_START, Configuration.Zero),
      DefaultCommandTimeout)

  def genericApply(configuration: Configuration): F[CommandResult] =
    giapi.command(
      Command(
        SequenceCommand.APPLY,
        Activity.PRESET_START,
        configuration
      ), DefaultCommandTimeout)
}

object GHOSTExample extends App {

  import cats.effect.IO
  import scala.concurrent.duration._

  private val ghostStatus =
    Stream.bracket(
      Giapi
        .giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)",
        scala.concurrent.ExecutionContext.Implicits.global)
        .connect)(
      _ => {
        val r: IO[Unit] = IO.unit
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  private val ghostSequence =
    Stream.bracket(
      Giapi
        .giapiConnection[IO]("failover:(tcp://127.0.0.1:61616)",
        scala.concurrent.ExecutionContext.Implicits.global)
        .connect)(
      giapi => {
        val client =
          new GHOSTClient[IO](giapi)
        val r = for {
          f <- client.observe("TEST_S20180509", 5.seconds)
        } yield f
        Stream.eval(r.map(println)) // scalastyle:ignore
      },
      _.close
    )

  (ghostStatus ++ ghostSequence).compile.drain.unsafeRunSync()
}
