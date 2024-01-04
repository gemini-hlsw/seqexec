// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.igrins2

import cats.Monad
import cats.effect.Resource
import cats.syntax.all._
import giapi.client.Giapi
import giapi.client.GiapiClient
import giapi.client.GiapiStatusDb
import giapi.client.syntax.status._
import cats.effect.Temporal
import cats.effect.kernel.Async
import fs2.Stream
import org.typelevel.log4cats.Logger

sealed trait Igrins2Client[F[_]] extends GiapiClient[F] {

  def statusDb: GiapiStatusDb[F]

  def requestedTime: F[Option[Float]]

  def exposureProgress: F[Stream[F, Int]]

  def currentStatus: F[String]

}

object Igrins2Client {
  val TimeProgress: String  = "ig2:is:timeprogress"
  val CurrentStatus: String = "ig2:is:currentstatus"
  val ObsTime: String       = "ig2:is:obstime"

  /**
   * Client for Igrins2
   */
  final private class Igrins2ClientImpl[F[_]: Monad: Logger](
    override val giapi: Giapi[F],
    val statusDb:       GiapiStatusDb[F]
  ) extends Igrins2Client[F] {
    def exposureProgress: F[Stream[F, Int]] =
      giapi.stream[Int](TimeProgress)

    def currentStatus: F[String] =
      statusDb.optional(CurrentStatus).map(_.stringValue.orEmpty)

    def requestedTime: F[Option[Float]] =
      statusDb.optional(ObsTime).map(_.floatValue)

  }

  // Used for simulations
  def simulatedIgrins2Client[F[_]: Temporal: Logger]: Resource[F, Igrins2Client[F]] =
    Resource.eval(
      Giapi
        .simulatedGiapiConnection[F]
        .connect
        .map(new Igrins2ClientImpl[F](_, GiapiStatusDb.simulatedDb[F]))
    )

  def igrins2Client[F[_]: Async: Logger](
    url: String
  ): Resource[F, Igrins2Client[F]] = {
    val giapi: Resource[F, Giapi[F]] =
      Resource.make(
        Giapi.giapiConnection[F](url).connect
      )(_.close)

    val db: Resource[F, GiapiStatusDb[F]] =
      Resource.make(
        GiapiStatusDb
          .newStatusDb[F](url, List(TimeProgress, CurrentStatus, ObsTime))
      )(_.close)

    (giapi, db).mapN(new Igrins2ClientImpl[F](_, _)).widen[Igrins2Client[F]]
  }

}
