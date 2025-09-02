// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s.encoder

import cats.effect.Concurrent
import org.http4s.EntityDecoder
import org.http4s.EntityEncoder
import org.http4s.booPickle.instances.BooPickleInstances
import seqexec.model.Conditions
import seqexec.model.Observation
import seqexec.model.Operator
import seqexec.model._
import seqexec.model.boopickle.ModelBooPicklers
import seqexec.model.enum.CloudCover
import seqexec.model.enum.ImageQuality
import seqexec.model.enum.SkyBackground
import seqexec.model.enum.WaterVapor

/**
 * Contains http4s implicit encoders of model objects
 */
trait BooEncoders extends ModelBooPicklers with BooPickleInstances {
  // Decoders, Included here instead of the on the object definitions to avoid
  // a circular dependency on http4s
  implicit def usrLoginDecoder[F[_]: Concurrent]: EntityDecoder[F, UserLoginRequest] =
    booOf[F, UserLoginRequest]
  implicit def userDetailEncoder[F[_]]: EntityEncoder[F, UserDetails]                =
    booEncoderOf[F, UserDetails]
  implicit def operatorEncoder[F[_]]: EntityEncoder[F, Operator]                     =
    booEncoderOf[F, Operator]
  implicit def idEncoder[F[_]: Concurrent]: EntityDecoder[F, Observation.Id]         =
    booOf[F, Observation.Id]
  implicit def lidEncoder[F[_]: Concurrent]: EntityDecoder[F, List[Observation.Id]]  =
    booOf[F, List[Observation.Id]]
  implicit def sIdEncoder[F[_]]: EntityEncoder[F, SequencesQueue[Observation.Id]]    =
    booEncoderOf[F, SequencesQueue[Observation.Id]]
  implicit def conditionsEncoder[F[_]: Concurrent]: EntityDecoder[F, Conditions]     =
    booOf[F, Conditions]
  implicit def iqEncoder[F[_]: Concurrent]: EntityDecoder[F, ImageQuality]           =
    booOf[F, ImageQuality]
  implicit def wvEncoder[F[_]: Concurrent]: EntityDecoder[F, WaterVapor]             =
    booOf[F, WaterVapor]
  implicit def sbEncoder[F[_]: Concurrent]: EntityDecoder[F, SkyBackground]          =
    booOf[F, SkyBackground]
  implicit def ccEncoder[F[_]: Concurrent]: EntityDecoder[F, CloudCover]             =
    booOf[F, CloudCover]
}

/**
 * Contains http4s implicit encoders of model objects, from the point of view of a client
 */
trait ClientBooEncoders extends ModelBooPicklers with BooPickleInstances {
  implicit def usrLoginEncoder[F[_]]: EntityEncoder[F, UserLoginRequest] =
    booEncoderOf[F, UserLoginRequest]
  implicit def wvEncoder[F[_]]: EntityEncoder[F, WaterVapor]             =
    booEncoderOf[F, WaterVapor]
  implicit def iqEncoder[F[_]]: EntityEncoder[F, ImageQuality]           =
    booEncoderOf[F, ImageQuality]
  implicit def sbEncoder[F[_]]: EntityEncoder[F, SkyBackground]          =
    booEncoderOf[F, SkyBackground]
  implicit def ccEncoder[F[_]]: EntityEncoder[F, CloudCover]             =
    booEncoderOf[F, CloudCover]
}
