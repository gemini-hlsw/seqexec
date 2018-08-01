// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.effect.IO
import cats.tests.CatsSuite
import edu.gemini.aspen.giapi.status.impl.BasicStatus
import edu.gemini.aspen.giapi.util.jms.JmsKeys
import edu.gemini.aspen.gmp.statusdb.StatusDatabase
import edu.gemini.aspen.gmp.statusgw.jms.JmsStatusDispatcher
import edu.gemini.aspen.gmp.statusgw.jms.StatusItemRequestListener
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider
import edu.gemini.jms.api.{
  BaseMessageConsumer,
  DestinationData,
  DestinationType,
  JmsSimpleMessageSelector
}
import fs2.Stream

final case class GmpStatus(amq: ActiveMQJmsProvider,
                     dispatcher: JmsStatusDispatcher,
                     msgConsumer: BaseMessageConsumer)

object GmpStatus {

  def amqUrl(name: String): String =
    s"vm://$name?broker.useJmx=false&marshal=false&broker.persistent=false"

  def amqUrlConnect(name: String): String =
    s"vm://$name?marshal=false&broker.persistent=false&create=false"

  /**
    * Setup a mini gmp that can store and provide status items
    */
  def createGmpStatus(amqUrl: String,
                intItemName: String,
                strItemName: String): IO[GmpStatus] = IO.apply {
    // Local in memory broker
    val amq = new ActiveMQJmsProvider(amqUrl)
    amq.startConnection()

    // Setup status listeners and db to answer status queries
    val database   = new StatusDatabase()
    val dispatcher = new JmsStatusDispatcher("Status Dispatcher")
    dispatcher.startJms(amq)
    val msgConsumer = new BaseMessageConsumer(
      "Status Consumer",
      new DestinationData(JmsKeys.GW_STATUS_REQUEST_DESTINATION,
                          DestinationType.TOPIC),
      new StatusItemRequestListener(database, dispatcher),
      new JmsSimpleMessageSelector(
        JmsKeys.GW_STATUS_REQUEST_TYPE_PROPERTY + " = '" + JmsKeys.GW_STATUS_REQUEST_TYPE_ITEM + "'")
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
final class GiapiStatusSpec extends CatsSuite {
  val intItemName = "item:a"
  val strItemName = "item:b"

  test("Test reading an existing status item") {
    val result = Stream.bracket(
      GmpStatus.createGmpStatus(GmpStatus.amqUrl("tests1"), intItemName, strItemName))(
      _ =>
        Stream.bracket(
          Giapi
            .giapiConnection[IO](GmpStatus.amqUrlConnect("tests1"))
            .connect)(c => Stream.eval(c.get[Int](intItemName)), _.close),
      GmpStatus.closeGmpStatus
    )
    result.compile.last.unsafeRunSync should contain(1)
  }

  test("Test reading an status with string type") {
    val result = Stream.bracket(
      GmpStatus.createGmpStatus(GmpStatus.amqUrl("tests2"), intItemName, strItemName))(
      _ =>
        Stream.bracket(
          Giapi
            .giapiConnection[IO](GmpStatus.amqUrlConnect("tests2"))
            .connect)(c => Stream.eval(c.get[String](strItemName)), _.close),
      GmpStatus.closeGmpStatus
    )
    result.compile.last.attempt.unsafeRunSync should matchPattern {
      case Right(Some("one")) =>
    }
  }

  test("Test reading an unknown status item") {
    val result = Stream.bracket(
      GmpStatus.createGmpStatus(GmpStatus.amqUrl("tests3"), intItemName, strItemName))(
      _ =>
        Stream.bracket(
          Giapi
            .giapiConnection[IO](GmpStatus.amqUrlConnect("tests3"))
            .connect)(c => Stream.eval(c.get[Int]("item:u")), _.close),
      GmpStatus.closeGmpStatus
    )
    result.compile.drain.attempt.unsafeRunSync should matchPattern {
      case Left(GiapiException(_)) =>
    }
  }

  test("Test reading an unknown status item as optional") {
    val result = Stream.bracket(
      GmpStatus.createGmpStatus(GmpStatus.amqUrl("tests4"), intItemName, strItemName))(
      _ =>
        Stream.bracket(
          Giapi
            .giapiConnection[IO](GmpStatus.amqUrlConnect("tests4"))
            .connect)(c => Stream.eval(c.getO[Int]("item:u")), _.close),
      GmpStatus.closeGmpStatus
    )
    result.compile.last.unsafeRunSync should matchPattern {
      case Some(None) =>
    }
  }

  test("Closing connection should terminate") {
    // This should fail but we are mostly concerned with ensuring that it terminates
    val result = Stream.bracket(
      GmpStatus.createGmpStatus(GmpStatus.amqUrl("tests5"), intItemName, strItemName))(
      g =>
        Stream.bracket(
          Giapi
            .giapiConnection[IO](GmpStatus.amqUrlConnect("tests5"))
            .connect)(
          c => Stream.eval(GmpStatus.closeGmpStatus(g) >> c.get[Int](intItemName)),
          _.close),
      GmpStatus.closeGmpStatus
    )
    result.compile.drain.attempt.unsafeRunSync should matchPattern {
      case Left(GiapiException(_)) =>
    }
  }
}
