// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import edu.gemini.spModel.gemini.gpi.Gpi.{ Apodizer => LegacyApodizer }
import edu.gemini.spModel.gemini.gpi.Gpi.{ FPM => LegacyFPM }
import edu.gemini.spModel.gemini.gpi.Gpi.{ Lyot => LegacyLyot }
import edu.gemini.spModel.gemini.gpi.Gpi.{ ObservingMode => LegacyObservingMode }

trait GpiLookupTables {
  val UNKNOWN_SETTING = "UNKNOWN"

  val apodizerLUT: Map[LegacyApodizer, String] = Map(
    LegacyApodizer.CLEAR     -> "CLEAR",
    LegacyApodizer.CLEARGP   -> "CLEARGP",
    LegacyApodizer.APOD_Y    -> "APOD_Y",
    LegacyApodizer.APOD_J    -> "APOD_J",
    LegacyApodizer.APOD_H    -> "APOD_H",
    LegacyApodizer.APOD_K1   -> "APOD_K1",
    LegacyApodizer.APOD_K2   -> "APOD_K2",
    LegacyApodizer.NRM       -> "NRM",
    LegacyApodizer.APOD_HL   -> "APOD_HL",
    LegacyApodizer.APOD_STAR -> "ND3",
    LegacyApodizer.ND3       -> "ND3"
  )

  val apodizerLUTNames: Map[String, String] =
    apodizerLUT.map { case (k, v) => (k.name, v) }

  val fpmLUT: Map[LegacyFPM, String] = Map(
    LegacyFPM.OPEN     -> "Open",
    LegacyFPM.F50umPIN -> "50umPIN",
    LegacyFPM.WITH_DOT -> "WITH_DOT",
    LegacyFPM.FPM_Y    -> "FPM_Y",
    LegacyFPM.FPM_J    -> "FPM_J",
    LegacyFPM.FPM_H    -> "FPM_H",
    LegacyFPM.FPM_K1   -> "FPM_K1",
    LegacyFPM.SCIENCE  -> "SCIENCE"
  )

  val lyotLUT: Map[LegacyLyot, String] = Map(
    LegacyLyot.OPEN              -> "Open",
    LegacyLyot.BLANK             -> "Blank",
    LegacyLyot.LYOT_080m12_03    -> "080m12_03",
    LegacyLyot.LYOT_080m12_04    -> "080m12_04",
    LegacyLyot.LYOT_080_04       -> "080_04",
    LegacyLyot.LYOT_080m12_06    -> "080m12_06",
    LegacyLyot.LYOT_080m12_04_c  -> "080m12_04_c",
    LegacyLyot.LYOT_080m12_06_03 -> "080m12_06_03",
    LegacyLyot.LYOT_080m12_07    -> "080m12_07",
    LegacyLyot.LYOT_080m12_10    -> "080m12_10"
  )

  val obsModeLUT: Map[LegacyObservingMode, String] = Map(
    LegacyObservingMode.CORON_Y_BAND   -> "Y_coron",
    LegacyObservingMode.CORON_J_BAND   -> "J_coron",
    LegacyObservingMode.CORON_H_BAND   -> "H_coron",
    LegacyObservingMode.CORON_K1_BAND  -> "K1_coron",
    LegacyObservingMode.CORON_K2_BAND  -> "K2_coron",
    LegacyObservingMode.H_STAR         -> "H_starcor",
    LegacyObservingMode.H_LIWA         -> "H_LIWAcor",
    LegacyObservingMode.DIRECT_Y_BAND  -> "Y_direct",
    LegacyObservingMode.DIRECT_J_BAND  -> "J_direct",
    LegacyObservingMode.DIRECT_H_BAND  -> "H_direct",
    LegacyObservingMode.DIRECT_K1_BAND -> "K1_direct",
    LegacyObservingMode.DIRECT_K2_BAND -> "K2_direct",
    LegacyObservingMode.NRM_Y          -> "NRM_Y",
    LegacyObservingMode.NRM_J          -> "NRM_J",
    LegacyObservingMode.NRM_H          -> "NRM_H",
    LegacyObservingMode.NRM_K1         -> "NRM_K1",
    LegacyObservingMode.NRM_K2         -> "NRM_K2",
    LegacyObservingMode.DARK           -> "DARK",
    LegacyObservingMode.UNBLOCKED_Y    -> "Y_unblocked",
    LegacyObservingMode.UNBLOCKED_J    -> "J_unblocked",
    LegacyObservingMode.UNBLOCKED_H    -> "H_unblocked",
    LegacyObservingMode.UNBLOCKED_K1   -> "K1_unblocked",
    LegacyObservingMode.UNBLOCKED_K2   -> "K2_unblocked"
  )
}
