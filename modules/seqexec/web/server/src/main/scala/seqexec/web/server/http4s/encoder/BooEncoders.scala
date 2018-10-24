// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s.encoder

import _root_.boopickle.Default._
import cats.effect.Sync
import gem.Observation
import org.http4s.EntityDecoder
import org.http4s.EntityEncoder
import org.http4s.booPickle._
import seqexec.model._
import seqexec.model.enum.CloudCover
import seqexec.model.enum.ImageQuality
import seqexec.model.enum.SkyBackground
import seqexec.model.enum.WaterVapor
import seqexec.model.Conditions
import seqexec.model.Operator
import seqexec.web.common.LogMessage
import seqexec.web.common.LogMessage._
import seqexec.web.model.boopickle.GemModelBooPicklers

/**
  * Contains http4s implicit encoders of model objects
  */
@SuppressWarnings(
  Array(
    "org.wartremover.warts.Equals",
    "org.wartremover.warts.ImplicitParameter",
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.OptionPartial"
  ))
trait BooEncoders extends GemModelBooPicklers {
  // Decoders, Included here instead of the on the object definitions to avoid
  // a circular dependency on http4s
  implicit def usrLoginDecoder[F[_]: Sync]: EntityDecoder[F, UserLoginRequest] =
    booOf[F, UserLoginRequest]
  implicit def userDetailEncoder[F[_]: Sync]: EntityEncoder[F, UserDetails] =
    booEncoderOf[F, UserDetails]
  implicit def operatorEncoder[F[_]: Sync]: EntityEncoder[F, Operator] =
    booEncoderOf[F, Operator]
  implicit def logMessageDecoder[F[_]: Sync]: EntityDecoder[F, LogMessage] =
    booOf[F, LogMessage]
  implicit def idEncoder[F[_]: Sync]: EntityDecoder[F, Observation.Id] =
    booOf[F, Observation.Id]
  implicit def lidEncoder[F[_]: Sync]: EntityDecoder[F, List[Observation.Id]] =
    booOf[F, List[Observation.Id]]
  implicit def sIdEncoder[F[_]: Sync]: EntityEncoder[F, SequencesQueue[Observation.Id]] =
    booEncoderOf[F, SequencesQueue[Observation.Id]]
  implicit def conditionsEncoder[F[_]: Sync]: EntityDecoder[F, Conditions] =
    booOf[F, Conditions]
  implicit def iqEncoder[F[_]: Sync]: EntityDecoder[F, ImageQuality] =
    booOf[F, ImageQuality]
  implicit def wvEncoder[F[_]: Sync]: EntityDecoder[F, WaterVapor] =
    booOf[F, WaterVapor]
  implicit def sbEncoder[F[_]: Sync]: EntityDecoder[F, SkyBackground] =
    booOf[F, SkyBackground]
  implicit def ccEncoder[F[_]: Sync]: EntityDecoder[F, CloudCover] =
    booOf[F, CloudCover]
}
