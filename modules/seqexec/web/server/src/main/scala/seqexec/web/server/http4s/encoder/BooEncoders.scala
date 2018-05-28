// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s.encoder

import seqexec.model._
import seqexec.model.Model.{CloudCover, Conditions, ImageQuality, Operator, SequenceId, SequencesQueue, SkyBackground, WaterVapor}
import boopickle.Default._
import cats.effect.Sync
import seqexec.web.common.{CliCommand, LogMessage}
import seqexec.web.common.LogMessage._
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.booPickle._

/**
  * Contains http4s implicit encoders of model objects
  */
@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.OptionPartial"))
trait BooEncoders {
  // Decoders, Included here instead of the on the object definitions to avoid
  // a circular dependency on http4s
  implicit def userLoginDecoder[F[_]: Sync]: EntityDecoder[F, UserLoginRequest] = booOf[F, UserLoginRequest]
  implicit def userDetailEncoder[F[_]: Sync]: EntityEncoder[F, UserDetails] = booEncoderOf[F, UserDetails]
  implicit def operatorEncoder[F[_]: Sync]: EntityEncoder[F, Operator] = booEncoderOf[F, Operator]
  implicit def logMessageDecoder[F[_]: Sync]: EntityDecoder[F, LogMessage] = booOf[F, LogMessage]
  implicit def commandsEncoder[F[_]: Sync]: EntityEncoder[F, CliCommand] = booEncoderOf[F, CliCommand]
  implicit def sequenceIdEncoder[F[_]: Sync]: EntityEncoder[F, SequencesQueue[SequenceId]] = booEncoderOf[F, SequencesQueue[SequenceId]]
  implicit def conditionsEncoder[F[_]: Sync]: EntityDecoder[F, Conditions] = booOf[F, Conditions]
  implicit def iqEncoder[F[_]: Sync]: EntityDecoder[F, ImageQuality] = booOf[F, ImageQuality]
  implicit def wvEncoder[F[_]: Sync]: EntityDecoder[F, WaterVapor] = booOf[F, WaterVapor]
  implicit def sbEncoder[F[_]: Sync]: EntityDecoder[F, SkyBackground] = booOf[F, SkyBackground]
  implicit def ccEncoder[F[_]: Sync]: EntityDecoder[F, CloudCover] = booOf[F, CloudCover]
}
