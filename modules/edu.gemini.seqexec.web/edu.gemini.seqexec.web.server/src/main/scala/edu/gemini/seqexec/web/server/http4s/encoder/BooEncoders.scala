// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server.http4s.encoder

import edu.gemini.seqexec.model._
import edu.gemini.seqexec.model.Model.{CloudCover, Conditions, ImageQuality, SequenceId, SequencesQueue, SkyBackground, WaterVapor, Operator}
import edu.gemini.seqexec.web.common._
import edu.gemini.seqexec.web.common.LogMessage._
import CliCommand._

import boopickle.Default._

/**
  * Contains http4s implicit encoders of model objects
  */
@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.OptionPartial", "org.wartremover.warts.Throw"))
trait BooEncoders {
  // Decoders, Included here instead of the on the object definitions to avoid
  // a circular dependency on http4s
  implicit val userLoginDecoder      = booOf[UserLoginRequest]
  implicit val userDetailEncoder     = booEncoderOf[UserDetails]
  implicit val operatorEncoder       = booEncoderOf[Operator]
  implicit val logMessageDecoder     = booOf[LogMessage]
  implicit val commandsEncoder       = booEncoderOf[CliCommand]
  implicit val sequenceIdEncoder     = booEncoderOf[SequencesQueue[SequenceId]]
  implicit val conditionsEncoder     = booOf[Conditions]
  implicit val iqEncoder             = booOf[ImageQuality]
  implicit val wvEncoder             = booOf[WaterVapor]
  implicit val sbEncoder             = booOf[SkyBackground]
  implicit val ccEncoder             = booOf[CloudCover]
}
