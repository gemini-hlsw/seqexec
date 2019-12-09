// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.implicits._
import edu.gemini.spModel.obscomp.InstConstants._
import edu.gemini.spModel.gemini.altair.AltairConstants
import edu.gemini.spModel.ao.AOConstants._
import edu.gemini.spModel.core.Wavelength
import seqexec.model.enum.Instrument
import seqexec.model.StepState
import seqexec.server.ConfigUtilOps._
import seqexec.server.flamingos2.Flamingos2
import seqexec.server.gpi.Gpi
import seqexec.server.ghost.Ghost
import seqexec.server.gsaoi._
import seqexec.server.gmos.GmosNorth
import seqexec.server.gmos.GmosSouth
import seqexec.server.niri._
import seqexec.server.nifs._
import seqexec.server.gnirs._
import seqexec.server.SeqexecFailure.UnrecognizedInstrument
import seqexec.server.SeqexecFailure.Unexpected
import seqexec.server.CleanConfig.extractItem

trait SequenceConfiguration {
  def extractInstrument(config: CleanConfig): Either[SeqexecFailure, Instrument] =
    config
      .extractInstAs[String](INSTRUMENT_NAME_PROP)
      .asTrySeq
      .flatMap {
        case Flamingos2.name => Instrument.F2.asRight
        case GmosSouth.name  => Instrument.GmosS.asRight
        case GmosNorth.name  => Instrument.GmosN.asRight
        case Gnirs.name      => Instrument.Gnirs.asRight
        case Gpi.name        => Instrument.Gpi.asRight
        case Ghost.name      => Instrument.Ghost.asRight
        case Niri.name       => Instrument.Niri.asRight
        case Nifs.name       => Instrument.Nifs.asRight
        case Gsaoi.name      => Instrument.Gsaoi.asRight
        case ins             => UnrecognizedInstrument(s"inst $ins").asLeft
      }

  def extractStatus(config: CleanConfig): StepState =
    config.extractObsAs[String](STATUS_PROP).map {
      case "ready"    => StepState.Pending
      case "complete" => StepState.Completed
      case "skipped"  => StepState.Skipped
      case kw         => StepState.Failed("Unexpected status keyword: " ++ kw)
    }.getOrElse(StepState.Failed("Logical error reading step status"))

  def extractWavelength(config: CleanConfig): Option[Wavelength] =
    config.extractAs[Wavelength](OBSERVING_WAVELENGTH_KEY).toOption

  def calcStepType(config: CleanConfig, isNightSeq: Boolean): TrySeq[StepType] = {
    def extractGaos(inst: Instrument): TrySeq[StepType] =
      config.extractAs[String](AO_SYSTEM_KEY) match {
        case Left(ConfigUtilOps.ConversionError(_, _)) =>
          Unexpected("Unable to get AO system from sequence").asLeft
        case Left(ConfigUtilOps.ContentError(_)) =>
          Unexpected("Logical error").asLeft
        case Left(ConfigUtilOps.KeyNotFound(_)) =>
          StepType.CelestialObject(inst).asRight
        case Right(AltairConstants.SYSTEM_NAME_PROP) =>
          StepType.AltairObs(inst).asRight
        case Right(edu.gemini.spModel.gemini.gems.Gems.SYSTEM_NAME) =>
          StepType.Gems(inst).asRight
        case _ =>
          Unexpected("Logical error reading AO system name").asLeft
      }

    (config
       .extractObsAs[String](OBSERVE_TYPE_PROP)
       .leftMap(explainExtractError),
     extractInstrument(config)).mapN { (obsType, inst) =>
      obsType match {
        case SCIENCE_OBSERVE_TYPE                                    => extractGaos(inst)
        case BIAS_OBSERVE_TYPE | DARK_OBSERVE_TYPE                   => inst match {
          case Instrument.GmosN | Instrument.GmosS => StepType.ExclusiveDarkOrBias(inst).asRight
          case _                                   => StepType.DarkOrBias(inst).asRight
        }
        case FLAT_OBSERVE_TYPE | ARC_OBSERVE_TYPE | CAL_OBSERVE_TYPE =>
          if(isNightSeq && inst.hasOI) StepType.NightFlatOrArc(inst).asRight
          else StepType.FlatOrArc(inst).asRight
        case _ => Unexpected("Unknown step type " + obsType).asLeft
      }
    }.flatten
  }

}

object SequenceConfiguration extends SequenceConfiguration
