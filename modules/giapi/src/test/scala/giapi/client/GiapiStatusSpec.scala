// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.effect.{ IO, Resource }
import munit.CatsEffectSuite
import edu.gemini.aspen.giapi.status.impl.BasicStatus
import edu.gemini.aspen.giapi.util.jms.JmsKeys
import edu.gemini.aspen.gmp.statusdb.StatusDatabase
import edu.gemini.aspen.gmp.statusgw.jms.JmsStatusDispatcher
import edu.gemini.aspen.gmp.statusgw.jms.StatusItemRequestListener
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider
import edu.gemini.jms.api.BaseMessageConsumer
import edu.gemini.jms.api.DestinationData
import edu.gemini.jms.api.DestinationType
import edu.gemini.jms.api.JmsSimpleMessageSelector
import org.scalatest.matchers.must.Matchers.matchPattern

final case class GmpStatus(
  amq:         ActiveMQJmsProvider,
  dispatcher:  JmsStatusDispatcher,
  msgConsumer: BaseMessageConsumer
)

object GmpStatus {

  def amqUrl(name: String): String =
    s"vm://$name?broker.useJmx=false&marshal=false&broker.persistent=false"

  def amqUrlConnect(name: String): String =
    s"vm://$name?marshal=false&broker.persistent=false&create=false"

  /**
   * Setup a mini gmp that can store and provide status items
   */
  def createGmpStatus(amqUrl: String, intItemName: String, strItemName: String): IO[GmpStatus] =
    IO.apply {
      // Local in memory broker
      val amq = new ActiveMQJmsProvider(amqUrl)
      amq.startConnection()

      // Setup status listeners and db to answer status queries
      val database    = new StatusDatabase()
      val dispatcher  = new JmsStatusDispatcher("Status Dispatcher")
      dispatcher.startJms(amq)
      val msgConsumer = new BaseMessageConsumer(
        "Status Consumer",
        new DestinationData(JmsKeys.GW_STATUS_REQUEST_DESTINATION, DestinationType.TOPIC),
        new StatusItemRequestListener(database, dispatcher),
        new JmsSimpleMessageSelector(
          JmsKeys.GW_STATUS_REQUEST_TYPE_PROPERTY + " = '" + JmsKeys.GW_STATUS_REQUEST_TYPE_ITEM + "'"
        )
      )

      msgConsumer.startJms(amq)
      // Set a status item
      database.update(new BasicStatus[Int](intItemName, 1))
      database.update(new BasicStatus[String](strItemName, "one"))
      GmpStatus(amq, dispatcher, msgConsumer)
    }

  def closeGmpStatus(gmp: GmpStatus): IO[Unit] = IO.apply {
    gmp.dispatcher.stopJms()
    gmp.msgConsumer.stopJms()
    gmp.amq.stopConnection()
  }
}

/**
 * Tests of the giapi api
 */
final class GiapiStatusSpec extends CatsEffectSuite {
  val intItemName = "item:a"
  val strItemName = "item:b"

  def client(
    amqUrl:      String,
    intItemName: String,
    strItemName: String
  ): Resource[IO, (GmpStatus, Giapi[IO])] =
    for {
      g <- Resource.make(GmpStatus.createGmpStatus(amqUrl, intItemName, strItemName))(
             GmpStatus.closeGmpStatus
           )
      c <- Resource.make(Giapi.giapiConnection[IO](amqUrl).connect)(_.close)
    } yield (g, c)

  test("Test reading an existing status item") {
    client(GmpStatus.amqUrl("tests1"), intItemName, strItemName)
      .use { case (_, c) =>
        c.get[Int](intItemName)
      }
      .map(assertEquals(_, 1))
  }

  test("Test reading an status with string type") {
    client(GmpStatus.amqUrl("tests2"), intItemName, strItemName)
      .use { case (_, c) =>
        c.get[String](strItemName)
      }
      .attempt
      .map(assertEquals(_, Right("one")))
  }

  test("Test reading an unknown status item") {
    client(GmpStatus.amqUrl("tests3"), intItemName, strItemName)
      .use { case (_, c) =>
        c.get[Int]("item:u")
      }
      .attempt
      .map(matchPattern { case Left(GiapiException(_)) => })
  }

  test("Test reading an unknown status item as optional") {
    client(GmpStatus.amqUrl("tests4"), intItemName, strItemName)
      .use { case (_, c) =>
        c.getO[Int]("item:u")
      }
      .map(assertEquals(_, None))
  }

  test("Closing connection should terminate") {
    // This should fail but we are mostly concerned with ensuring that it terminates
    client(GmpStatus.amqUrl("tests5"), intItemName, strItemName)
      .use { case (g, c) =>
        GmpStatus.closeGmpStatus(g) >> c.get[Int](intItemName)
      }
      .attempt
      .map(matchPattern { case Left(GiapiException(_)) => })
  }

}
