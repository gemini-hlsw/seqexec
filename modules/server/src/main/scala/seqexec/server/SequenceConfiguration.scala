// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.syntax.all._
import edu.gemini.spModel.ao.AOConstants._
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.gemini.altair.AltairConstants
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.{ Wavelength => GNIRSWavelength }
import edu.gemini.spModel.obscomp.InstConstants._
import seqexec.model.StepState
import seqexec.model.enum.Instrument
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigUtilOps._
import seqexec.server.SeqexecFailure.Unexpected
import seqexec.server.SeqexecFailure.UnrecognizedInstrument
import seqexec.server.flamingos2.Flamingos2
import seqexec.server.ghost.Ghost
import seqexec.server.gmos.GmosNorth
import seqexec.server.gmos.GmosSouth
import seqexec.server.gnirs._
import seqexec.server.gpi.Gpi
import seqexec.server.gsaoi._
import seqexec.server.nifs._
import seqexec.server.niri._

trait SequenceConfiguration {
  def extractInstrument(config: CleanConfig): Either[SeqexecFailure, Instrument] =
    config
      .extractInstAs[String](INSTRUMENT_NAME_PROP)
      .adaptExtractFailure
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
    config
      .extractObsAs[String](STATUS_PROP)
      .map {
        case "ready"    => StepState.Pending
        case "complete" => StepState.Completed
        case "skipped"  => StepState.Skipped
        case kw         => StepState.Failed("Unexpected status keyword: " ++ kw)
      }
      .getOrElse(StepState.Failed("Logical error reading step status"))

  /**
   * Attempts to extract the Wavelength from the sequence. The value is not always present thus we
   * can get a None Also errors reading the value are possible thus we produce an Either
   */
  def extractWavelength(config: CleanConfig): Either[SeqexecFailure, Option[Wavelength]] =
    if (!config.containsKey(OBSERVING_WAVELENGTH_KEY))
      none.asRight
    else
      // Gmos uses String
      config
        .extractAs[String](OBSERVING_WAVELENGTH_KEY)
        .flatMap(v => Either.catchNonFatal(v.toDouble).leftMap(_ => new ContentError(v)))
        .map(Wavelength.fromMicrons[Double](_).some)
        .orElse {
          // GNIRS uses its own wavelength!!
          config
            .extractAs[GNIRSWavelength](OBSERVING_WAVELENGTH_KEY)
            .map(w => Wavelength.fromMicrons(w.doubleValue()).some)
        }
        .orElse {
          // Maybe we use ocs Wavelength
          config
            .extractAs[Wavelength](OBSERVING_WAVELENGTH_KEY)
            .map(_.some)
        }
        .orElse {
          // Just in case
          config
            .extractAs[java.lang.Double](OBSERVING_WAVELENGTH_KEY)
            .map(v => Wavelength.fromMicrons[Double](v.doubleValue).some)
        }
        .leftMap { case _ =>
          Unexpected(
            s"Error reading wavelength ${config.itemValue(OBSERVING_WAVELENGTH_KEY)}: ${config.itemValue(OBSERVING_WAVELENGTH_KEY).getClass()}"
          )
        }

  def calcStepType(config: CleanConfig, isNightSeq: Boolean): Either[SeqexecFailure, StepType] = {
    def extractGaos(inst: Instrument): Either[SeqexecFailure, StepType] =
      config.extractAs[String](AO_SYSTEM_KEY) match {
        case Left(ConfigUtilOps.ConversionError(_, _))              =>
          Unexpected("Unable to get AO system from sequence").asLeft
        case Left(ConfigUtilOps.ContentError(_))                    =>
          Unexpected("Logical error").asLeft
        case Left(ConfigUtilOps.KeyNotFound(_))                     =>
          StepType.CelestialObject(inst).asRight
        case Right(AltairConstants.SYSTEM_NAME_PROP)                =>
          StepType.AltairObs(inst).asRight
        case Right(edu.gemini.spModel.gemini.gems.Gems.SYSTEM_NAME) =>
          StepType.Gems(inst).asRight
        case _                                                      =>
          Unexpected("Logical error reading AO system name").asLeft
      }

    (config
       .extractObsAs[String](OBSERVE_TYPE_PROP)
       .leftMap(explainExtractError),
     extractInstrument(config)
    ).mapN { (obsType, inst) =>
      obsType match {
        case SCIENCE_OBSERVE_TYPE                                    => extractGaos(inst)
        case BIAS_OBSERVE_TYPE | DARK_OBSERVE_TYPE                   =>
          inst match {
            case Instrument.GmosN | Instrument.GmosS => StepType.ExclusiveDarkOrBias(inst).asRight
            case _                                   => StepType.DarkOrBias(inst).asRight
          }
        case FLAT_OBSERVE_TYPE | ARC_OBSERVE_TYPE | CAL_OBSERVE_TYPE =>
          if (isNightSeq && inst.hasOI) StepType.NightFlatOrArc(inst).asRight
          else StepType.FlatOrArc(inst).asRight
        case _                                                       => Unexpected("Unknown step type " + obsType).asLeft
      }
    }.flatten
  }

}

object SequenceConfiguration extends SequenceConfiguration
