// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi

import scala.concurrent.duration._

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.syntax.all._
import edu.gemini.aspen.giapi.commands.HandlerResponse.Response
import edu.gemini.aspen.giapi.commands.SequenceCommand
import edu.gemini.aspen.giapi.status.StatusHandler
import edu.gemini.aspen.giapi.status.StatusItem
import edu.gemini.aspen.giapi.statusservice.StatusHandlerAggregate
import edu.gemini.aspen.giapi.statusservice.StatusService
import edu.gemini.aspen.giapi.util.jms.status.StatusGetter
import edu.gemini.aspen.gmp.commands.jms.client.CommandSenderClient
import edu.gemini.jms.activemq.provider.ActiveMQJmsProvider
import fs2.Stream
import fs2.concurrent
import fs2.concurrent.Queue
import giapi.client.commands.Command
import giapi.client.commands.CommandResultException
import giapi.client.commands._
import shapeless.Typeable._
import cats.effect.Temporal

package object client {

  /**
   * Allowed types according to GIAPI
   */
  implicit val strItemGetter: ItemGetter[String] = new ItemGetter[String] {}

  implicit val doubleItemGetter: ItemGetter[Double] = new ItemGetter[Double] {}

  implicit val intItemGetter: ItemGetter[Int] = new ItemGetter[Int] {}

  implicit val floatItemGetter: ItemGetter[Float] = new ItemGetter[Float] {}
}

package client {

  import giapi.client.commands.CommandResult

  final case class GiapiException(str: String) extends RuntimeException(str)

  /**
   * Typeclass to present as evidence when calling `Giapi.get`
   */
  sealed abstract class ItemGetter[A: shapeless.Typeable] {

    /**
     * Attempt to convert any value to A as sent by StatusHandler
     */
    def value(p: Any): Option[A] = shapeless.Typeable[A].cast(p)
  }

  object ItemGetter {

    @inline
    def apply[F](implicit instance: ItemGetter[F]): ItemGetter[F] = instance
  }

  /**
   * Represents a connection to a GIAPi based instrument
   *
   * @tparam F Effect type
   */
  trait GiapiConnection[F[_]] {
    def connect: F[Giapi[F]]
  }

  /**
   * Algebra to interact with a GIAPI instrument
   *
   * @tparam F Effect Type
   */
  trait Giapi[F[_]]           {

    /**
     * Returns a value for the status item. If not found or there is an error, an exception could be thrown
     */
    def get[A: ItemGetter](statusItem: String): F[A]

    /**
     * Attempts to read a value. If not found an empty F is returned
     */
    def getO[A: ItemGetter](statusItem: String): F[Option[A]]

    /**
     * Executes a command as defined on GIAPI
     * Note that commands can end in ERROR or COMPLETED
     * Giapi has an extra case where we have a command ACCEPTED and it will complete in the future
     * That makes handling easier with callbacks on Java land but on IO-land it makes more sense to
     * wait for ERROR/COMPLETED and do async calls above this level
     *
     * This decision may be review in the future
     */
    def command(command: Command, timeout: FiniteDuration): F[CommandResult]

    /**
     * Returns a stream of values for the status item.
     */
    def stream[A: ItemGetter](statusItem: String): F[Stream[F, A]]

    /**
     * Close the connection
     */
    private[giapi] def close: F[Unit]
  }

  /**
   * Interpreters
   */
  object Giapi {

    final case class StatusStreamer(aggregate: StatusHandlerAggregate, ss: StatusService)

    def statusGetter[F[_]: Sync](c: ActiveMQJmsProvider): F[StatusGetter] =
      Sync[F].delay {
        val sg = new StatusGetter("statusGetter")
        sg.startJms(c)
        sg
      }

    private def commandSenderClient[F[_]: Applicative](
      c: ActiveMQJmsProvider
    ): F[CommandSenderClient] =
      Applicative[F].pure {
        new CommandSenderClient(c)
      }

    def statusStreamer[F[_]: Sync](c: ActiveMQJmsProvider): F[StatusStreamer] =
      Sync[F].delay {
        val aggregate     = new StatusHandlerAggregate()
        val statusService = new StatusService(aggregate, "statusService", "*")
        statusService.startJms(c)
        StatusStreamer(aggregate, statusService)
      }

    private def streamItem[F[_]: ConcurrentEffect, A: ItemGetter](
      agg:        StatusHandlerAggregate,
      statusItem: String
    ): F[Stream[F, A]] =
      Sync[F].delay {

        def statusHandler(q: concurrent.Queue[F, A]) = new StatusHandler {

          override def update[B](item: StatusItem[B]): Unit =
            // Check the item name and attempt convert it to A
            if (item.getName === statusItem) {
              ItemGetter[A].value(item.getValue).foreach { a =>
                q.enqueue1(a).toIO.unsafeRunAsync(_ => ())
              }
            }

          override def getName: String = "StatusHandler"
        }

        // A trivial resource that binds and unbinds a status handler.
        def bind(q: Queue[F, A]): Resource[F, StatusHandler] =
          Resource.make(
            Effect[F].delay {
              val sh = statusHandler(q)
              agg.bindStatusHandler(sh)
              sh
            }
          )(sh => Effect[F].delay(agg.unbindStatusHandler(sh)))

        // Put the items in a queue as they arrive to the stream
        for {
          q <- Stream.eval(Queue.unbounded[F, A])
          _ <- Stream.resource(bind(q))
          i <- q.dequeue
        } yield i
      }

    /**
     * Interpreter on F
     *
     * @param url Url to connect to
     * @tparam F Effect type
     */
    def giapiConnection[F[_]: Temporal: ConcurrentEffect](
      url: String
    ): GiapiConnection[F] =
      new GiapiConnection[F] {
        private def giapi(
          c:  ActiveMQJmsProvider,
          sg: StatusGetter,
          cc: CommandSenderClient,
          ss: StatusStreamer
        ) =
          new Giapi[F] {
            private val commandsAckTimeout = 2000.milliseconds

            override def get[A: ItemGetter](statusItem: String): F[A] =
              getO[A](statusItem).flatMap {
                case Some(a) => a.pure[F]
                case None    =>
                  Sync[F].raiseError(new GiapiException(s"Status item $statusItem not found"))
              }

            def getO[A: ItemGetter](statusItem: String): F[Option[A]] =
              Sync[F].delay {
                val item = sg.getStatusItem[A](statusItem)
                Option(item).map(_.getValue)
              }

            override def command(command: Command, timeOut: FiniteDuration): F[CommandResult] = {
              val error = CommandResultException.timedOut(timeOut)
              val e     = ApplicativeError[F, Throwable].raiseError[CommandResult](error)
              commands.sendCommand(cc, command, commandsAckTimeout).timeoutTo(timeOut, e)
            }

            override def stream[A: ItemGetter](statusItem: String): F[Stream[F, A]] =
              streamItem[F, A](ss.aggregate, statusItem)

            override def close: F[Unit] =
              for {
                _ <- Sync[F].delay(sg.stopJms())
                _ <- Sync[F].delay(ss.ss.stopJms())
                _ <- Sync[F].delay(c.stopConnection())
              } yield ()

          }

        private def build(c: ActiveMQJmsProvider): F[Giapi[F]] =
          for {
            sg <- statusGetter[F](c)
            cc <- commandSenderClient[F](c)
            ss <- statusStreamer[F](c)
          } yield giapi(c, sg, cc, ss)

        def connect: F[Giapi[F]] =
          for {
            c <- Sync[F].delay(new ActiveMQJmsProvider(url)) // Build the connection
            _ <- Sync[F].delay(c.startConnection()) // Start the connection
            c <- build(c) // Build the interpreter
          } yield c
      }

    /**
     * Interpreter on Id
     */
    def giapiConnectionId: GiapiConnection[Id] = new GiapiConnection[Id] {
      override def connect: Id[Giapi[Id]] = new Giapi[Id] {
        override def get[A: ItemGetter](statusItem:    String): Id[A]         =
          sys.error(s"Cannot read $statusItem")
        override def getO[A: ItemGetter](statusItem:   String): Id[Option[A]] = None
        override def stream[A: ItemGetter](statusItem: String): Id[Stream[Id, A]] =
          sys.error(s"Cannot read $statusItem")
        override def command(command:                  Command, timeout: FiniteDuration): Id[CommandResult] =
          CommandResult(Response.COMPLETED)
        override def close: Id[Unit] = ()
      }
    }

    /**
     * Simulator interpreter on IO, Reading items will fail and all commands will succeed
     */
    def simulatedGiapiConnection[F[_]](implicit
      T: Temporal[F],
      F: ApplicativeError[F, Throwable]
    ): GiapiConnection[F] = new GiapiConnection[F] {
      override def connect: F[Giapi[F]] = F.pure(new Giapi[F] {
        override def get[A: ItemGetter](statusItem:    String): F[A]         =
          F.raiseError(new RuntimeException(s"Cannot read $statusItem"))
        override def getO[A: ItemGetter](statusItem:   String): F[Option[A]] = F.pure(None)
        override def stream[A: ItemGetter](statusItem: String): F[Stream[F, A]] =
          F.pure(Stream.empty.covary[F])
        override def command(command:                  Command, timeout: FiniteDuration): F[CommandResult] =
          if (command.sequenceCommand === SequenceCommand.OBSERVE) {
            T.sleep(timeout) *>
              F.pure(CommandResult(Response.COMPLETED))
          } else {
            T.sleep(5.seconds) *>
              F.pure(CommandResult(Response.COMPLETED))
          }
        override def close: F[Unit] = F.unit
      })
    }

  }

}
