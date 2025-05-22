// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.igrins2

import cats.effect.Sync
import cats.syntax.all._
import fs2.Stream
import giapi.client.GiapiClient
import giapi.client.commands.Configuration
import giapi.client.igrins2.Igrins2Client
import lucuma.core.util.Enumerated
import org.typelevel.log4cats.Logger
import seqexec.server.AbstractGiapiInstrumentController
import seqexec.server.GiapiInstrumentController
import seqexec.server.keywords.GdsClient
import squants.time.Seconds

trait Igrins2Config {
  def configuration: Configuration

}

trait Igrins2Controller[F[_]] extends GiapiInstrumentController[F, Igrins2Config] {
  def gdsClient: GdsClient[F]

  def exposureProgress: F[Stream[F, Int]]

  def sequenceComplete: F[Unit]

  def requestedTime: F[Option[Float]]

  def currentStatus: F[Igrins2ControllerState]

  def dcIsPreparing: F[Boolean]

  def dcIsAcquiring: F[Boolean]

  def dcIsReadingOut: F[Boolean]

  def dcIsWritingMEF: F[Boolean]

  def abort: F[Unit]

}

sealed trait Igrins2ControllerState extends Product with Serializable {
  def tag: String
}

object Igrins2ControllerState {
  case object Idle        extends Igrins2ControllerState {
    override def tag: String = "IDLE"
  }
  case object Exposing    extends Igrins2ControllerState {
    override def tag: String = "EXPOSING"
  }
  case object ReadingOut  extends Igrins2ControllerState {
    override def tag: String = "FOWLER_BACK"
  }
  case object CreatingMEF extends Igrins2ControllerState {
    override def tag: String = "CREATING MEF"
  }
  case object Error       extends Igrins2ControllerState {
    override def tag: String = "ERROR"
  }

  def fromString(s: String): Option[Igrins2ControllerState] =
    all.find(_.tag === s)

  val all = List(Idle, Exposing, ReadingOut, CreatingMEF, Error)

  implicit val igrins2ControllerStateEnum: Enumerated[Igrins2ControllerState] =
    Enumerated.of(Idle, Exposing, ReadingOut, CreatingMEF, Error)
}

object Igrins2Controller {
  def apply[F[_]: Sync: Logger](client: Igrins2Client[F], gds: GdsClient[F]): Igrins2Controller[F] =
    new AbstractGiapiInstrumentController[F, Igrins2Config, Igrins2Client[F]](
      client,
      Seconds(GiapiClient.DefaultCommandTimeout.toSeconds)
    ) with Igrins2Controller[F] {
      override val gdsClient: GdsClient[F] = gds

      override val name = "IGRINS-2"

      override def exposureProgress: F[Stream[F, Int]] =
        client.exposureProgress

      override def sequenceComplete: F[Unit] =
        client.sequenceComplete

      def requestedTime: F[Option[Float]] = client.requestedTime

      def currentStatus: F[Igrins2ControllerState] =
        client.currentStatus
          .map(Igrins2ControllerState.fromString)
          .map(_.getOrElse(Igrins2ControllerState.Error))

      def dcIsPreparing: F[Boolean] = currentStatus.map(_ === Igrins2ControllerState.Idle)

      def dcIsAcquiring: F[Boolean] =
        currentStatus
          .map(_ === Igrins2ControllerState.Exposing)

      def dcIsReadingOut: F[Boolean] = false.pure[F]

      def dcIsWritingMEF: F[Boolean] = currentStatus.map(_ === Igrins2ControllerState.CreatingMEF)

      override def abort: F[Unit] = client.abort.void

      override def configuration(config: Igrins2Config): F[Configuration] =
        config.configuration.pure[F]
    }
}
