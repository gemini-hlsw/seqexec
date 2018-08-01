// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.effect.IO
import cats.tests.CatsSuite
import giapi.client.commands._
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider
import edu.gemini.aspen.giapi.commands.{Activity, Command => JCommand, SequenceCommand, CompletionListener, HandlerResponse}
import edu.gemini.aspen.giapi.commands.HandlerResponse.Response
import edu.gemini.aspen.gmp.commands.jms.clientbridge.CommandMessagesBridgeImpl
import edu.gemini.aspen.gmp.commands.jms.clientbridge.CommandMessagesConsumer
import edu.gemini.aspen.giapi.commands.CommandSender
import fs2.Stream
import scala.concurrent.duration._
import org.scalatest.EitherValues

final case class GmpCommands(amq: ActiveMQJmsProvider, cmc: CommandMessagesConsumer)

object GmpCommands {

  def amqUrl(name: String): String =
    s"vm://$name?broker.useJmx=false&marshal=false&broker.persistent=false"

  def amqUrlConnect(name: String): String =
    s"vm://$name?marshal=false&broker.persistent=false&create=false"

  /**
    * Setup a mini gmp that can store and provide status items
    */
  def createGmpCommands(amqUrl: String, handleCommands: Boolean): IO[GmpCommands] = IO.apply {
    // Local in memory broker
    val amq = new ActiveMQJmsProvider(amqUrl)
    amq.startConnection()
    val cs = new CommandSender() {
      // This is essentially an instrument simulator
      // we test several possible scenarios
      override def sendCommand(command: JCommand, listener: CompletionListener): HandlerResponse =
        sendCommand(command, listener, 0)

      @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
      override def sendCommand(command: JCommand, listener: CompletionListener, timeout: Long): HandlerResponse =
        command.getSequenceCommand match {
          case SequenceCommand.INIT => HandlerResponse.COMPLETED
          case SequenceCommand.PARK => HandlerResponse.ACCEPTED
          case _                    => HandlerResponse.NOANSWER
        }

    }
    val cmb = new CommandMessagesBridgeImpl(amq, cs)
    val commandMessagesConsumer = new CommandMessagesConsumer(cmb)
    if (handleCommands) {
      commandMessagesConsumer.startJms(amq)
    }

    GmpCommands(amq, commandMessagesConsumer)
  }

  def closeGmpCommands(gmp: GmpCommands): IO[Unit] = IO.apply {
    gmp.cmc.stopJms()
    gmp.amq.stopConnection()
  }
}

/**
  * Tests of the giapi api
  */
final class GiapiCommandSpec extends CatsSuite with EitherValues {

//  test("Test sending a command with no handlers") {
//    val result = Stream.bracket(
//      GmpCommands.createGmpCommands(GmpCommands.amqUrl("test1"), false))(
//      _ =>
//        Stream.bracket(
//          Giapi
//            .giapiConnection[IO](GmpCommands.amqUrlConnect("test1"), 2000.millis)
//            .connect)(c => Stream.eval(c.command(Command(SequenceCommand.TEST, Activity.PRESET, Configuration.Zero)).attempt), _.close),
//      GmpCommands.closeGmpCommands
//    )
//    result.compile.last.unsafeRunSync.map(_.left.value) should contain(CommandResultException(Response.ERROR, "Message cannot be null"))
//  }

  test("Test sending a command with no answer") {
    val result = Stream.bracket(
      GmpCommands.createGmpCommands(GmpCommands.amqUrl("test2"), true))(
      _ =>
        Stream.bracket(
          Giapi
            .giapiConnection[IO](GmpCommands.amqUrlConnect("test2"), 2000.millis)
            .connect)(c => Stream.eval(c.command(Command(SequenceCommand.TEST, Activity.PRESET, Configuration.Zero)).attempt), _.close),
      GmpCommands.closeGmpCommands
    )
    result.compile.last.unsafeRunSync.map(_.left.value) should contain(CommandResultException(Response.NOANSWER, "No answer from the instrument"))
  }

  test("Test sending a command with immediate answer") {
    val result = Stream.bracket(
      GmpCommands.createGmpCommands(GmpCommands.amqUrl("test3"), true))(
      _ =>
        Stream.bracket(
          Giapi
            .giapiConnection[IO](GmpCommands.amqUrlConnect("test3"), 2000.millis)
            .connect)(c => Stream.eval(c.command(Command(SequenceCommand.INIT, Activity.PRESET, Configuration.Zero)).attempt), _.close),
      GmpCommands.closeGmpCommands
    )
    result.compile.last.unsafeRunSync.map(_.right.value) should contain(CommandResult(Response.COMPLETED))
  }

  test("Test sending a command with accepted but never completed answer") {
    val result = Stream.bracket(
      GmpCommands.createGmpCommands(GmpCommands.amqUrl("test4"), true))(
      _ =>
        Stream.bracket(
          Giapi
            .giapiConnection[IO](GmpCommands.amqUrlConnect("test4"), 2000.millis)
            .connect)(c => Stream.eval(c.command(Command(SequenceCommand.PARK, Activity.PRESET, Configuration.Zero)).attempt), _.close),
      GmpCommands.closeGmpCommands
    )
    result.compile.last.unsafeRunSync.map(_.left.value) should contain(CommandResultException.timedOut(5.seconds))
  }
}
