// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.effect.{ IO, Resource }
import munit.CatsEffectSuite
import giapi.client.commands._
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider
import edu.gemini.aspen.giapi.commands.{
  Activity,
  Command => JCommand,
  CompletionListener,
  HandlerResponse,
  SequenceCommand
}
import edu.gemini.aspen.giapi.commands.HandlerResponse.Response
import edu.gemini.aspen.gmp.commands.jms.clientbridge.CommandMessagesBridgeImpl
import edu.gemini.aspen.gmp.commands.jms.clientbridge.CommandMessagesConsumer
import edu.gemini.aspen.giapi.commands.CommandSender
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
    val amq                     = new ActiveMQJmsProvider(amqUrl)
    amq.startConnection()
    val cs                      = new CommandSender() {
      // This is essentially an instrument simulator
      // we test several possible scenarios
      override def sendCommand(command: JCommand, listener: CompletionListener): HandlerResponse =
        sendCommand(command, listener, 0)

      override def sendCommand(
        command:  JCommand,
        listener: CompletionListener,
        timeout:  Long
      ): HandlerResponse =
        command.getSequenceCommand match {
          case SequenceCommand.INIT => HandlerResponse.COMPLETED
          case SequenceCommand.PARK => HandlerResponse.ACCEPTED
          case _                    => HandlerResponse.NOANSWER
        }

    }
    val cmb                     = new CommandMessagesBridgeImpl(amq, cs)
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
final class GiapiCommandSpec extends CatsEffectSuite with EitherValues {

  def client(amqUrl: String, handleCommands: Boolean): Resource[IO, Giapi[IO]] =
    for {
      _ <- Resource.make(GmpCommands.createGmpCommands(amqUrl, handleCommands))(
             GmpCommands.closeGmpCommands
           )
      c <- Resource.make(Giapi.giapiConnection[IO](amqUrl).connect)(_.close)
    } yield c

  test("Test sending a command with no handlers".ignore) { // This test passes but the backend doesn't clean up properly
    client(GmpCommands.amqUrl("test1"), false)
      .use { c =>
        c.command(Command(SequenceCommand.TEST, Activity.PRESET, Configuration.Zero), 1.second)
          .attempt
      }
      .map(assertEquals(_, Left(CommandResultException(Response.ERROR, "Message cannot be null"))))
  }

  test("Test sending a command with no answer") {
    client(GmpCommands.amqUrl("test2"), true)
      .use { c =>
        c.command(Command(SequenceCommand.TEST, Activity.PRESET, Configuration.Zero), 1.second)
          .attempt
      }
      .map(
        assertEquals(
          _,
          Left(CommandResultException(Response.NOANSWER, "No answer from the instrument"))
        )
      )
  }

  test("Test sending a command with immediate answer") {
    client(GmpCommands.amqUrl("test3"), true)
      .use { c =>
        c.command(Command(SequenceCommand.INIT, Activity.PRESET, Configuration.Zero), 1.second)
          .attempt
      }
      .map(assertEquals(_, Right(CommandResult(Response.COMPLETED))))
  }

  test("Test sending a command with accepted but never completed answer") {
    val timeout = 1.second
    client(GmpCommands.amqUrl("test4"), true)
      .use { c =>
        c.command(Command(SequenceCommand.PARK, Activity.PRESET, Configuration.Zero), timeout)
          .attempt
      }
      .map(assertEquals(_, Left(CommandResultException.timedOut(timeout))))
  }

}
