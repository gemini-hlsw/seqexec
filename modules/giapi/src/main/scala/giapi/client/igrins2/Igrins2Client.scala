// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.igrins2

import cats.effect.Resource
import cats.syntax.all._
import giapi.client.Giapi
import giapi.client.GiapiClient
import giapi.client.GiapiStatusDb
import cats.effect.Temporal
import cats.effect.kernel.Async

sealed trait Igrins2Client[F[_]] extends GiapiClient[F] {

  def statusDb: GiapiStatusDb[F]
}

object Igrins2Client {

  /**
   * Client for GPI
   */
  final private class Igrins2ClientImpl[F[_]](
    override val giapi: Giapi[F],
    val statusDb:       GiapiStatusDb[F]
  ) extends Igrins2Client[F]

  // Used for simulations
  def simulatedIgrins2Client[F[_]: Temporal]: Resource[F, Igrins2Client[F]] =
    Resource.eval(
      Giapi
        .simulatedGiapiConnection[F]
        .connect
        .map(new Igrins2ClientImpl[F](_, GiapiStatusDb.simulatedDb[F]))
    )

  def igrins2Client[F[_]: Async](
    url:               String,
    statusesToMonitor: List[String]
  ): Resource[F, Igrins2Client[F]] = {
    val giapi: Resource[F, Giapi[F]] =
      Resource.make(
        Giapi.giapiConnection[F](url).connect
      )(_.close)

    val db: Resource[F, GiapiStatusDb[F]] =
      Resource.make(
        GiapiStatusDb
          .newStatusDb[F](url, statusesToMonitor)
      )(_.close)

    (giapi, db).mapN(new Igrins2ClientImpl[F](_, _)).widen[Igrins2Client[F]]
  }

}
