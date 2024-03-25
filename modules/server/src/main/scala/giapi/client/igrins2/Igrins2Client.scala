// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.igrins2

import cats._
import cats.effect._
import cats.syntax.all._
import giapi.client.commands.Command
import edu.gemini.aspen.giapi.commands.Activity
import edu.gemini.aspen.giapi.commands.SequenceCommand
import giapi.client._
import giapi.client.syntax.status._
import giapi.client.commands.Configuration
import fs2._

sealed trait Igrins2Client[F[_]] extends GiapiClient[F] {

  def statusDb: GiapiStatusDb[F]

  def requestedTime: F[Option[Float]]

  def exposureProgress: F[Stream[F, Int]]

  def currentStatus: F[String]

  def sequenceComplete: F[Unit]

}

object Igrins2Client {
  val TimeProgress: String  = "ig2:is:timeprogress"
  val CurrentStatus: String = "ig2:is:currentstatus"
  val ObsTime: String       = "ig2:is:obstime"

  /**
   * Client for Igrins2
   */
  final private class Igrins2ClientImpl[F[_]: Monad](
    override val giapi: Giapi[F],
    val statusDb:       GiapiStatusDb[F]
  ) extends Igrins2Client[F] {
    def exposureProgress: F[Stream[F, Int]] =
      giapi.stream[Int](TimeProgress)

    def currentStatus: F[String] =
      statusDb.optional(CurrentStatus).map(_.stringValue.orEmpty)

    def requestedTime: F[Option[Float]] =
      statusDb.optional(ObsTime).map(_.floatValue)

    def sequenceComplete: F[Unit] =
      giapi
        .command(Command(
                   SequenceCommand.ENGINEERING,
                   Activity.PRESET_START,
                   Configuration.single("COMMAND_NAME", "testFast")
                 ),
                 GiapiClient.DefaultCommandTimeout
        )
        .void
  }

  // Used for simulations
  def simulatedIgrins2Client[F[_]: Temporal]: Resource[F, Igrins2Client[F]] =
    Resource.eval(
      Giapi
        .simulatedGiapiConnection[F]
        .connect
        .map(new Igrins2ClientImpl[F](_, GiapiStatusDb.simulatedDb[F]))
    )

  def igrins2Client[F[_]: Async](
    url: String
  ): Resource[F, Igrins2Client[F]] = {
    val giapi: Resource[F, Giapi[F]] =
      Resource.make(
        Giapi.giapiConnection[F](url).connect
      )(_.close)

    val db: Resource[F, GiapiStatusDb[F]] =
      Resource.make(
        GiapiStatusDb
          .newStatusDb[F](url, List(TimeProgress, CurrentStatus, ObsTime), List("*"))
      )(_.close)

    (giapi, db).mapN(new Igrins2ClientImpl[F](_, _)).widen[Igrins2Client[F]]
  }

}
