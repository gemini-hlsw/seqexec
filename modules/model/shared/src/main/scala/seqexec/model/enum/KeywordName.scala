// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enums

import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Fits Keyword names.
 * @group Enumerations (Generated)
 */
sealed abstract class KeywordName(
  val tag:  String,
  val name: String
) extends Product
    with Serializable

object KeywordName {

  /** @group Constructors */
  case object INSTRUMENT extends KeywordName("INSTRUMENT", "INSTRUME")

  /** @group Constructors */
  case object SEQEXVER extends KeywordName("SEQEXVER", "SEQEXVER")

  /** @group Constructors */
  case object OBJECT extends KeywordName("OBJECT", "OBJECT")

  /** @group Constructors */
  case object OBSTYPE extends KeywordName("OBSTYPE", "OBSTYPE")

  /** @group Constructors */
  case object OBSCLASS extends KeywordName("OBSCLASS", "OBSCLASS")

  /** @group Constructors */
  case object GEMPRGID extends KeywordName("GEMPRGID", "GEMPRGID")

  /** @group Constructors */
  case object OBSID extends KeywordName("OBSID", "OBSID")

  /** @group Constructors */
  case object DATALAB extends KeywordName("DATALAB", "DATALAB")

  /** @group Constructors */
  case object OBSERVER extends KeywordName("OBSERVER", "OBSERVER")

  /** @group Constructors */
  case object OBSERVAT extends KeywordName("OBSERVAT", "OBSERVAT")

  /** @group Constructors */
  case object TELESCOP extends KeywordName("TELESCOP", "TELESCOP")

  /** @group Constructors */
  case object PARALLAX extends KeywordName("PARALLAX", "PARALLAX")

  /** @group Constructors */
  case object RADVEL extends KeywordName("RADVEL", "RADVEL")

  /** @group Constructors */
  case object EPOCH extends KeywordName("EPOCH", "EPOCH")

  /** @group Constructors */
  case object EQUINOX extends KeywordName("EQUINOX", "EQUINOX")

  /** @group Constructors */
  case object TRKEQUIN extends KeywordName("TRKEQUIN", "TRKEQUIN")

  /** @group Constructors */
  case object SSA extends KeywordName("SSA", "SSA")

  /** @group Constructors */
  case object RA extends KeywordName("RA", "RA")

  /** @group Constructors */
  case object DEC extends KeywordName("DEC", "DEC")

  /** @group Constructors */
  case object ELEVATIO extends KeywordName("ELEVATIO", "ELEVATIO")

  /** @group Constructors */
  case object AZIMUTH extends KeywordName("AZIMUTH", "AZIMUTH")

  /** @group Constructors */
  case object CRPA extends KeywordName("CRPA", "CRPA")

  /** @group Constructors */
  case object HA extends KeywordName("HA", "HA")

  /** @group Constructors */
  case object LT extends KeywordName("LT", "LT")

  /** @group Constructors */
  case object TRKFRAME extends KeywordName("TRKFRAME", "TRKFRAME")

  /** @group Constructors */
  case object DECTRACK extends KeywordName("DECTRACK", "DECTRACK")

  /** @group Constructors */
  case object TRKEPOCH extends KeywordName("TRKEPOCH", "TRKEPOCH")

  /** @group Constructors */
  case object RATRACK extends KeywordName("RATRACK", "RATRACK")

  /** @group Constructors */
  case object FRAME extends KeywordName("FRAME", "FRAME")

  /** @group Constructors */
  case object PMDEC extends KeywordName("PMDEC", "PMDEC")

  /** @group Constructors */
  case object PMRA extends KeywordName("PMRA", "PMRA")

  /** @group Constructors */
  case object WAVELENG extends KeywordName("WAVELENG", "WAVELENG")

  /** @group Constructors */
  case object RAWIQ extends KeywordName("RAWIQ", "RAWIQ")

  /** @group Constructors */
  case object RAWCC extends KeywordName("RAWCC", "RAWCC")

  /** @group Constructors */
  case object RAWWV extends KeywordName("RAWWV", "RAWWV")

  /** @group Constructors */
  case object RAWBG extends KeywordName("RAWBG", "RAWBG")

  /** @group Constructors */
  case object RAWPIREQ extends KeywordName("RAWPIREQ", "RAWPIREQ")

  /** @group Constructors */
  case object RAWGEMQA extends KeywordName("RAWGEMQA", "RAWGEMQA")

  /** @group Constructors */
  case object CGUIDMOD extends KeywordName("CGUIDMOD", "CGUIDMOD")

  /** @group Constructors */
  case object UT extends KeywordName("UT", "UT")

  /** @group Constructors */
  case object DATE extends KeywordName("DATE", "DATE")

  /** @group Constructors */
  case object M2BAFFLE extends KeywordName("M2BAFFLE", "M2BAFFLE")

  /** @group Constructors */
  case object M2CENBAF extends KeywordName("M2CENBAF", "M2CENBAF")

  /** @group Constructors */
  case object ST extends KeywordName("ST", "ST")

  /** @group Constructors */
  case object XOFFSET extends KeywordName("XOFFSET", "XOFFSET")

  /** @group Constructors */
  case object YOFFSET extends KeywordName("YOFFSET", "YOFFSET")

  /** @group Constructors */
  case object POFFSET extends KeywordName("POFFSET", "POFFSET")

  /** @group Constructors */
  case object QOFFSET extends KeywordName("QOFFSET", "QOFFSET")

  /** @group Constructors */
  case object RAOFFSET extends KeywordName("RAOFFSET", "RAOFFSET")

  /** @group Constructors */
  case object DECOFFSE extends KeywordName("DECOFFSE", "DECOFFSE")

  /** @group Constructors */
  case object RATRGOFF extends KeywordName("RATRGOFF", "RATRGOFF")

  /** @group Constructors */
  case object DECTRGOF extends KeywordName("DECTRGOF", "DECTRGOF")

  /** @group Constructors */
  case object PA extends KeywordName("PA", "PA")

  /** @group Constructors */
  case object IAA extends KeywordName("IAA", "IAA")

  /** @group Constructors */
  case object SFRT2 extends KeywordName("SFRT2", "SFRT2")

  /** @group Constructors */
  case object SFTILT extends KeywordName("SFTILT", "SFTILT")

  /** @group Constructors */
  case object SFLINEAR extends KeywordName("SFLINEAR", "SFLINEAR")

  /** @group Constructors */
  case object AOFOLD extends KeywordName("AOFOLD", "AOFOLD")

  /** @group Constructors */
  case object PWFS1_ST extends KeywordName("PWFS1_ST", "PWFS1_ST")

  /** @group Constructors */
  case object PWFS2_ST extends KeywordName("PWFS2_ST", "PWFS2_ST")

  /** @group Constructors */
  case object OIWFS_ST extends KeywordName("OIWFS_ST", "OIWFS_ST")

  /** @group Constructors */
  case object AOWFS_ST extends KeywordName("AOWFS_ST", "AOWFS_ST")

  /** @group Constructors */
  case object SCIBAND extends KeywordName("SCIBAND", "SCIBAND")

  /** @group Constructors */
  case object NUMREQTW extends KeywordName("NUMREQTW", "NUMREQTW")

  /** @group Constructors */
  case object REQIQ extends KeywordName("REQIQ", "REQIQ")

  /** @group Constructors */
  case object REQCC extends KeywordName("REQCC", "REQCC")

  /** @group Constructors */
  case object REQBG extends KeywordName("REQBG", "REQBG")

  /** @group Constructors */
  case object REQWV extends KeywordName("REQWV", "REQWV")

  /** @group Constructors */
  case object REQMAXAM extends KeywordName("REQMAXAM", "REQMAXAM")

  /** @group Constructors */
  case object REQMAXHA extends KeywordName("REQMAXHA", "REQMAXHA")

  /** @group Constructors */
  case object REQMINAM extends KeywordName("REQMINAM", "REQMINAM")

  /** @group Constructors */
  case object REQMINHA extends KeywordName("REQMINHA", "REQMINHA")

  /** @group Constructors */
  case object OIARA extends KeywordName("OIARA", "OIARA")

  /** @group Constructors */
  case object OIADEC extends KeywordName("OIADEC", "OIADEC")

  /** @group Constructors */
  case object OIARV extends KeywordName("OIARV", "OIARV")

  /** @group Constructors */
  case object OIAWAVEL extends KeywordName("OIAWAVEL", "OIAWAVEL")

  /** @group Constructors */
  case object OIAEPOCH extends KeywordName("OIAEPOCH", "OIAEPOCH")

  /** @group Constructors */
  case object OIAEQUIN extends KeywordName("OIAEQUIN", "OIAEQUIN")

  /** @group Constructors */
  case object OIAFRAME extends KeywordName("OIAFRAME", "OIAFRAME")

  /** @group Constructors */
  case object OIAOBJEC extends KeywordName("OIAOBJEC", "OIAOBJEC")

  /** @group Constructors */
  case object OIAPMDEC extends KeywordName("OIAPMDEC", "OIAPMDEC")

  /** @group Constructors */
  case object OIAPMRA extends KeywordName("OIAPMRA", "OIAPMRA")

  /** @group Constructors */
  case object OIAPARAL extends KeywordName("OIAPARAL", "OIAPARAL")

  /** @group Constructors */
  case object OIFOCUS extends KeywordName("OIFOCUS", "OIFOCUS")

  /** @group Constructors */
  case object P1ARA extends KeywordName("P1ARA", "P1ARA")

  /** @group Constructors */
  case object P1ADEC extends KeywordName("P1ADEC", "P1ADEC")

  /** @group Constructors */
  case object P1ARV extends KeywordName("P1ARV", "P1ARV")

  /** @group Constructors */
  case object P1AWAVEL extends KeywordName("P1AWAVEL", "P1AWAVEL")

  /** @group Constructors */
  case object P1AEPOCH extends KeywordName("P1AEPOCH", "P1AEPOCH")

  /** @group Constructors */
  case object P1AEQUIN extends KeywordName("P1AEQUIN", "P1AEQUIN")

  /** @group Constructors */
  case object P1AFRAME extends KeywordName("P1AFRAME", "P1AFRAME")

  /** @group Constructors */
  case object P1AOBJEC extends KeywordName("P1AOBJEC", "P1AOBJEC")

  /** @group Constructors */
  case object P1APMDEC extends KeywordName("P1APMDEC", "P1APMDEC")

  /** @group Constructors */
  case object P1APMRA extends KeywordName("P1APMRA", "P1APMRA")

  /** @group Constructors */
  case object P1APARAL extends KeywordName("P1APARAL", "P1APARAL")

  /** @group Constructors */
  case object P1FOCUS extends KeywordName("P1FOCUS", "P1FOCUS")

  /** @group Constructors */
  case object P2ARA extends KeywordName("P2ARA", "P2ARA")

  /** @group Constructors */
  case object P2ADEC extends KeywordName("P2ADEC", "P2ADEC")

  /** @group Constructors */
  case object P2ARV extends KeywordName("P2ARV", "P2ARV")

  /** @group Constructors */
  case object P2AWAVEL extends KeywordName("P2AWAVEL", "P2AWAVEL")

  /** @group Constructors */
  case object P2AEPOCH extends KeywordName("P2AEPOCH", "P2AEPOCH")

  /** @group Constructors */
  case object P2AEQUIN extends KeywordName("P2AEQUIN", "P2AEQUIN")

  /** @group Constructors */
  case object P2AFRAME extends KeywordName("P2AFRAME", "P2AFRAME")

  /** @group Constructors */
  case object P2AOBJEC extends KeywordName("P2AOBJEC", "P2AOBJEC")

  /** @group Constructors */
  case object P2APMDEC extends KeywordName("P2APMDEC", "P2APMDEC")

  /** @group Constructors */
  case object P2APMRA extends KeywordName("P2APMRA", "P2APMRA")

  /** @group Constructors */
  case object P2APARAL extends KeywordName("P2APARAL", "P2APARAL")

  /** @group Constructors */
  case object P2FOCUS extends KeywordName("P2FOCUS", "P2FOCUS")

  /** @group Constructors */
  case object AOARA extends KeywordName("AOARA", "AOARA")

  /** @group Constructors */
  case object AOADEC extends KeywordName("AOADEC", "AOADEC")

  /** @group Constructors */
  case object AOARV extends KeywordName("AOARV", "AOARV")

  /** @group Constructors */
  case object AOAWAVEL extends KeywordName("AOAWAVEL", "AOAWAVEL")

  /** @group Constructors */
  case object AOAEPOCH extends KeywordName("AOAEPOCH", "AOAEPOCH")

  /** @group Constructors */
  case object AOAEQUIN extends KeywordName("AOAEQUIN", "AOAEQUIN")

  /** @group Constructors */
  case object AOAFRAME extends KeywordName("AOAFRAME", "AOAFRAME")

  /** @group Constructors */
  case object AOAOBJEC extends KeywordName("AOAOBJEC", "AOAOBJEC")

  /** @group Constructors */
  case object AOAPMDEC extends KeywordName("AOAPMDEC", "AOAPMDEC")

  /** @group Constructors */
  case object AOAPMRA extends KeywordName("AOAPMRA", "AOAPMRA")

  /** @group Constructors */
  case object AOAPARAL extends KeywordName("AOAPARAL", "AOAPARAL")

  /** @group Constructors */
  case object AOFOCUS extends KeywordName("AOFOCUS", "AOFOCUS")

  /** @group Constructors */
  case object OIFREQ extends KeywordName("OIFREQ", "OIFREQ")

  /** @group Constructors */
  case object P1FREQ extends KeywordName("P1FREQ", "P1FREQ")

  /** @group Constructors */
  case object P2FREQ extends KeywordName("P2FREQ", "P2FREQ")

  /** @group Constructors */
  case object AIRMASS extends KeywordName("AIRMASS", "AIRMASS")

  /** @group Constructors */
  case object AMSTART extends KeywordName("AMSTART", "AMSTART")

  /** @group Constructors */
  case object AMEND extends KeywordName("AMEND", "AMEND")

  /** @group Constructors */
  case object PROP_MD extends KeywordName("PROP_MD", "PROP_MD")

  /** @group Constructors */
  case object RELEASE extends KeywordName("RELEASE", "RELEASE")

  /** @group Constructors */
  case object PREIMAGE extends KeywordName("PREIMAGE", "PREIMAGE")

  /** @group Constructors */
  case object DATE_OBS extends KeywordName("DATE_OBS", "DATE-OBS")

  /** @group Constructors */
  case object TIME_OBS extends KeywordName("TIME_OBS", "TIME-OBS")

  /** @group Constructors */
  case object READMODE extends KeywordName("READMODE", "READMODE")

  /** @group Constructors */
  case object NREADS extends KeywordName("NREADS", "NREADS")

  /** @group Constructors */
  case object GCALLAMP extends KeywordName("GCALLAMP", "GCALLAMP")

  /** @group Constructors */
  case object GCALFILT extends KeywordName("GCALFILT", "GCALFILT")

  /** @group Constructors */
  case object GCALDIFF extends KeywordName("GCALDIFF", "GCALDIFF")

  /** @group Constructors */
  case object GCALSHUT extends KeywordName("GCALSHUT", "GCALSHUT")

  /** @group Constructors */
  case object PAR_ANG extends KeywordName("PAR_ANG", "PAR_ANG")

  /** @group Constructors */
  case object INPORT extends KeywordName("INPORT", "INPORT")

  /** @group Constructors */
  case object ASTROMTC extends KeywordName("ASTROMTC", "ASTROMTC")

  /** @group Constructors */
  case object CRFOLLOW extends KeywordName("CRFOLLOW", "CRFOLLOW")

  /** @group Constructors */
  case object HUMIDITY extends KeywordName("HUMIDITY", "HUMIDITY")

  /** @group Constructors */
  case object TAMBIENT extends KeywordName("TAMBIENT", "TAMBIENT")

  /** @group Constructors */
  case object TAMBIEN2 extends KeywordName("TAMBIEN2", "TAMBIEN2")

  /** @group Constructors */
  case object PRESSURE extends KeywordName("PRESSURE", "PRESSURE")

  /** @group Constructors */
  case object PRESSUR2 extends KeywordName("PRESSUR2", "PRESSUR2")

  /** @group Constructors */
  case object DEWPOINT extends KeywordName("DEWPOINT", "DEWPOINT")

  /** @group Constructors */
  case object DEWPOIN2 extends KeywordName("DEWPOIN2", "DEWPOIN2")

  /** @group Constructors */
  case object WINDSPEE extends KeywordName("WINDSPEE", "WINDSPEE")

  /** @group Constructors */
  case object WINDSPE2 extends KeywordName("WINDSPE2", "WINDSPE2")

  /** @group Constructors */
  case object WINDDIRE extends KeywordName("WINDDIRE", "WINDDIRE")

  /** @group Constructors */
  case object ARRAYID extends KeywordName("ARRAYID", "ARRAYID")

  /** @group Constructors */
  case object ARRAYTYP extends KeywordName("ARRAYTYP", "ARRAYTYP")

  /** @group Constructors */
  case object UTSTART extends KeywordName("UTSTART", "UTSTART")

  /** @group Constructors */
  case object FILTER1 extends KeywordName("FILTER1", "FILTER1")

  /** @group Constructors */
  case object FW1_ENG extends KeywordName("FW1_ENG", "FW1_ENG")

  /** @group Constructors */
  case object FILTER2 extends KeywordName("FILTER2", "FILTER2")

  /** @group Constructors */
  case object FW2_ENG extends KeywordName("FW2_ENG", "FW2_ENG")

  /** @group Constructors */
  case object CAMERA extends KeywordName("CAMERA", "CAMERA")

  /** @group Constructors */
  case object CAM_ENG extends KeywordName("CAM_ENG", "CAM_ENG")

  /** @group Constructors */
  case object SLIT extends KeywordName("SLIT", "SLIT")

  /** @group Constructors */
  case object SLIT_ENG extends KeywordName("SLIT_ENG", "SLIT_ENG")

  /** @group Constructors */
  case object DECKER extends KeywordName("DECKER", "DECKER")

  /** @group Constructors */
  case object DKR_ENG extends KeywordName("DKR_ENG", "DKR_ENG")

  /** @group Constructors */
  case object GRATING extends KeywordName("GRATING", "GRATING")

  /** @group Constructors */
  case object GR_ENG extends KeywordName("GR_ENG", "GR_ENG")

  /** @group Constructors */
  case object GRATWAVE extends KeywordName("GRATWAVE", "GRATWAVE")

  /** @group Constructors */
  case object GRATORD extends KeywordName("GRATORD", "GRATORD")

  /** @group Constructors */
  case object GRATTILT extends KeywordName("GRATTILT", "GRATTILT")

  /** @group Constructors */
  case object PRISM extends KeywordName("PRISM", "PRISM")

  /** @group Constructors */
  case object PRSM_ENG extends KeywordName("PRSM_ENG", "PRSM_ENG")

  /** @group Constructors */
  case object ACQMIR extends KeywordName("ACQMIR", "ACQMIR")

  /** @group Constructors */
  case object COVER extends KeywordName("COVER", "COVER")

  /** @group Constructors */
  case object FOCUS extends KeywordName("FOCUS", "FOCUS")

  /** @group Constructors */
  case object FCS_ENG extends KeywordName("FCS_ENG", "FCS_ENG")

  /** @group Constructors */
  case object DETBIAS extends KeywordName("DETBIAS", "DETBIAS")

  /** @group Constructors */
  case object UTEND extends KeywordName("UTEND", "UTEND")

  /** @group Constructors */
  case object OBSEPOCH extends KeywordName("OBSEPOCH", "OBSEPOCH")

  /** @group Constructors */
  case object GMOSCC extends KeywordName("GMOSCC", "GMOSCC")

  /** @group Constructors */
  case object ADCENPST extends KeywordName("ADCENPST", "ADCENPST")

  /** @group Constructors */
  case object ADCENPEN extends KeywordName("ADCENPEN", "ADCENPEN")

  /** @group Constructors */
  case object ADCENPME extends KeywordName("ADCENPME", "ADCENPME")

  /** @group Constructors */
  case object ADCEXPST extends KeywordName("ADCEXPST", "ADCEXPST")

  /** @group Constructors */
  case object ADCEXPEN extends KeywordName("ADCEXPEN", "ADCEXPEN")

  /** @group Constructors */
  case object ADCEXPME extends KeywordName("ADCEXPME", "ADCEXPME")

  /** @group Constructors */
  case object ADCWLEN1 extends KeywordName("ADCWLEN1", "ADCWLEN1")

  /** @group Constructors */
  case object ADCWLEN2 extends KeywordName("ADCWLEN2", "ADCWLEN2")

  /** @group Constructors */
  case object MASKID extends KeywordName("MASKID", "MASKID")

  /** @group Constructors */
  case object MASKNAME extends KeywordName("MASKNAME", "MASKNAME")

  /** @group Constructors */
  case object MASKTYP extends KeywordName("MASKTYP", "MASKTYP")

  /** @group Constructors */
  case object MASKLOC extends KeywordName("MASKLOC", "MASKLOC")

  /** @group Constructors */
  case object FILTID1 extends KeywordName("FILTID1", "FILTID1")

  /** @group Constructors */
  case object FILTID2 extends KeywordName("FILTID2", "FILTID2")

  /** @group Constructors */
  case object GRATID extends KeywordName("GRATID", "GRATID")

  /** @group Constructors */
  case object GRWLEN extends KeywordName("GRWLEN", "GRWLEN")

  /** @group Constructors */
  case object CENTWAVE extends KeywordName("CENTWAVE", "CENTWAVE")

  /** @group Constructors */
  case object GRORDER extends KeywordName("GRORDER", "GRORDER")

  /** @group Constructors */
  case object GRTILT extends KeywordName("GRTILT", "GRTILT")

  /** @group Constructors */
  case object GRSTEP extends KeywordName("GRSTEP", "GRSTEP")

  /** @group Constructors */
  case object DTAX extends KeywordName("DTAX", "DTAX")

  /** @group Constructors */
  case object DTAY extends KeywordName("DTAY", "DTAY")

  /** @group Constructors */
  case object DTAZ extends KeywordName("DTAZ", "DTAZ")

  /** @group Constructors */
  case object DTAZST extends KeywordName("DTAZST", "DTAZST")

  /** @group Constructors */
  case object DTAZEN extends KeywordName("DTAZEN", "DTAZEN")

  /** @group Constructors */
  case object DTAZME extends KeywordName("DTAZME", "DTAZME")

  /** @group Constructors */
  case object DTMODE extends KeywordName("DTMODE", "DTMODE")

  /** @group Constructors */
  case object ADCMODE extends KeywordName("ADCMODE", "ADCMODE")

  /** @group Constructors */
  case object GMOSDC extends KeywordName("GMOSDC", "GMOSDC")

  /** @group Constructors */
  case object DETTYPE extends KeywordName("DETTYPE", "DETTYPE")

  /** @group Constructors */
  case object DETID extends KeywordName("DETID", "DETID")

  /** @group Constructors */
  case object EXPOSURE extends KeywordName("EXPOSURE", "EXPOSURE")

  /** @group Constructors */
  case object ADCUSED extends KeywordName("ADCUSED", "ADCUSED")

  /** @group Constructors */
  case object DETNROI extends KeywordName("DETNROI", "DETNROI")

  /** @group Constructors */
  case object DETRO0X extends KeywordName("DETRO0X", "DETRO0X")

  /** @group Constructors */
  case object DETRO0XS extends KeywordName("DETRO0XS", "DETRO0XS")

  /** @group Constructors */
  case object DETRO0Y extends KeywordName("DETRO0Y", "DETRO0Y")

  /** @group Constructors */
  case object DETRO0YS extends KeywordName("DETRO0YS", "DETRO0YS")

  /** @group Constructors */
  case object DETRO1X extends KeywordName("DETRO1X", "DETRO1X")

  /** @group Constructors */
  case object DETRO1XS extends KeywordName("DETRO1XS", "DETRO1XS")

  /** @group Constructors */
  case object DETRO1Y extends KeywordName("DETRO1Y", "DETRO1Y")

  /** @group Constructors */
  case object DETRO1YS extends KeywordName("DETRO1YS", "DETRO1YS")

  /** @group Constructors */
  case object DETRO2X extends KeywordName("DETRO2X", "DETRO2X")

  /** @group Constructors */
  case object DETRO2XS extends KeywordName("DETRO2XS", "DETRO2XS")

  /** @group Constructors */
  case object DETRO2Y extends KeywordName("DETRO2Y", "DETRO2Y")

  /** @group Constructors */
  case object DETRO2YS extends KeywordName("DETRO2YS", "DETRO2YS")

  /** @group Constructors */
  case object DETRO3X extends KeywordName("DETRO3X", "DETRO3X")

  /** @group Constructors */
  case object DETRO3XS extends KeywordName("DETRO3XS", "DETRO3XS")

  /** @group Constructors */
  case object DETRO3Y extends KeywordName("DETRO3Y", "DETRO3Y")

  /** @group Constructors */
  case object DETRO3YS extends KeywordName("DETRO3YS", "DETRO3YS")

  /** @group Constructors */
  case object DETRO4X extends KeywordName("DETRO4X", "DETRO4X")

  /** @group Constructors */
  case object DETRO4XS extends KeywordName("DETRO4XS", "DETRO4XS")

  /** @group Constructors */
  case object DETRO4Y extends KeywordName("DETRO4Y", "DETRO4Y")

  /** @group Constructors */
  case object DETRO4YS extends KeywordName("DETRO4YS", "DETRO4YS")

  /** @group Constructors */
  case object DETRO5X extends KeywordName("DETRO5X", "DETRO5X")

  /** @group Constructors */
  case object DETRO5XS extends KeywordName("DETRO5XS", "DETRO5XS")

  /** @group Constructors */
  case object DETRO5Y extends KeywordName("DETRO5Y", "DETRO5Y")

  /** @group Constructors */
  case object DETRO5YS extends KeywordName("DETRO5YS", "DETRO5YS")

  /** @group Constructors */
  case object DETRO6X extends KeywordName("DETRO6X", "DETRO6X")

  /** @group Constructors */
  case object DETRO6XS extends KeywordName("DETRO6XS", "DETRO6XS")

  /** @group Constructors */
  case object DETRO6Y extends KeywordName("DETRO6Y", "DETRO6Y")

  /** @group Constructors */
  case object DETRO6YS extends KeywordName("DETRO6YS", "DETRO6YS")

  /** @group Constructors */
  case object DETRO7X extends KeywordName("DETRO7X", "DETRO7X")

  /** @group Constructors */
  case object DETRO7XS extends KeywordName("DETRO7XS", "DETRO7XS")

  /** @group Constructors */
  case object DETRO7Y extends KeywordName("DETRO7Y", "DETRO7Y")

  /** @group Constructors */
  case object DETRO7YS extends KeywordName("DETRO7YS", "DETRO7YS")

  /** @group Constructors */
  case object DETRO8X extends KeywordName("DETRO8X", "DETRO8X")

  /** @group Constructors */
  case object DETRO8XS extends KeywordName("DETRO8XS", "DETRO8XS")

  /** @group Constructors */
  case object DETRO8Y extends KeywordName("DETRO8Y", "DETRO8Y")

  /** @group Constructors */
  case object DETRO8YS extends KeywordName("DETRO8YS", "DETRO8YS")

  /** @group Constructors */
  case object DETRO9X extends KeywordName("DETRO9X", "DETRO9X")

  /** @group Constructors */
  case object DETRO9XS extends KeywordName("DETRO9XS", "DETRO9XS")

  /** @group Constructors */
  case object DETRO9Y extends KeywordName("DETRO9Y", "DETRO9Y")

  /** @group Constructors */
  case object DETRO9YS extends KeywordName("DETRO9YS", "DETRO9YS")

  /** @group Constructors */
  case object REQTWS01 extends KeywordName("REQTWS01", "REQTWS01")

  /** @group Constructors */
  case object REQTWD01 extends KeywordName("REQTWD01", "REQTWD01")

  /** @group Constructors */
  case object REQTWN01 extends KeywordName("REQTWN01", "REQTWN01")

  /** @group Constructors */
  case object REQTWP01 extends KeywordName("REQTWP01", "REQTWP01")

  /** @group Constructors */
  case object REQTWS02 extends KeywordName("REQTWS02", "REQTWS02")

  /** @group Constructors */
  case object REQTWD02 extends KeywordName("REQTWD02", "REQTWD02")

  /** @group Constructors */
  case object REQTWN02 extends KeywordName("REQTWN02", "REQTWN02")

  /** @group Constructors */
  case object REQTWP02 extends KeywordName("REQTWP02", "REQTWP02")

  /** @group Constructors */
  case object REQTWS03 extends KeywordName("REQTWS03", "REQTWS03")

  /** @group Constructors */
  case object REQTWD03 extends KeywordName("REQTWD03", "REQTWD03")

  /** @group Constructors */
  case object REQTWN03 extends KeywordName("REQTWN03", "REQTWN03")

  /** @group Constructors */
  case object REQTWP03 extends KeywordName("REQTWP03", "REQTWP03")

  /** @group Constructors */
  case object REQTWS04 extends KeywordName("REQTWS04", "REQTWS04")

  /** @group Constructors */
  case object REQTWD04 extends KeywordName("REQTWD04", "REQTWD04")

  /** @group Constructors */
  case object REQTWN04 extends KeywordName("REQTWN04", "REQTWN04")

  /** @group Constructors */
  case object REQTWP04 extends KeywordName("REQTWP04", "REQTWP04")

  /** @group Constructors */
  case object REQTWS05 extends KeywordName("REQTWS05", "REQTWS05")

  /** @group Constructors */
  case object REQTWD05 extends KeywordName("REQTWD05", "REQTWD05")

  /** @group Constructors */
  case object REQTWN05 extends KeywordName("REQTWN05", "REQTWN05")

  /** @group Constructors */
  case object REQTWP05 extends KeywordName("REQTWP05", "REQTWP05")

  /** @group Constructors */
  case object REQTWS06 extends KeywordName("REQTWS06", "REQTWS06")

  /** @group Constructors */
  case object REQTWD06 extends KeywordName("REQTWD06", "REQTWD06")

  /** @group Constructors */
  case object REQTWN06 extends KeywordName("REQTWN06", "REQTWN06")

  /** @group Constructors */
  case object REQTWP06 extends KeywordName("REQTWP06", "REQTWP06")

  /** @group Constructors */
  case object REQTWS07 extends KeywordName("REQTWS07", "REQTWS07")

  /** @group Constructors */
  case object REQTWD07 extends KeywordName("REQTWD07", "REQTWD07")

  /** @group Constructors */
  case object REQTWN07 extends KeywordName("REQTWN07", "REQTWN07")

  /** @group Constructors */
  case object REQTWP07 extends KeywordName("REQTWP07", "REQTWP07")

  /** @group Constructors */
  case object REQTWS08 extends KeywordName("REQTWS08", "REQTWS08")

  /** @group Constructors */
  case object REQTWD08 extends KeywordName("REQTWD08", "REQTWD08")

  /** @group Constructors */
  case object REQTWN08 extends KeywordName("REQTWN08", "REQTWN08")

  /** @group Constructors */
  case object REQTWP08 extends KeywordName("REQTWP08", "REQTWP08")

  /** @group Constructors */
  case object REQTWS09 extends KeywordName("REQTWS09", "REQTWS09")

  /** @group Constructors */
  case object REQTWD09 extends KeywordName("REQTWD09", "REQTWD09")

  /** @group Constructors */
  case object REQTWN09 extends KeywordName("REQTWN09", "REQTWN09")

  /** @group Constructors */
  case object REQTWP09 extends KeywordName("REQTWP09", "REQTWP09")

  /** @group Constructors */
  case object REQTWS10 extends KeywordName("REQTWS10", "REQTWS10")

  /** @group Constructors */
  case object REQTWD10 extends KeywordName("REQTWD10", "REQTWD10")

  /** @group Constructors */
  case object REQTWN10 extends KeywordName("REQTWN10", "REQTWN10")

  /** @group Constructors */
  case object REQTWP10 extends KeywordName("REQTWP10", "REQTWP10")

  /** @group Constructors */
  case object REQTWS11 extends KeywordName("REQTWS11", "REQTWS11")

  /** @group Constructors */
  case object REQTWD11 extends KeywordName("REQTWD11", "REQTWD11")

  /** @group Constructors */
  case object REQTWN11 extends KeywordName("REQTWN11", "REQTWN11")

  /** @group Constructors */
  case object REQTWP11 extends KeywordName("REQTWP11", "REQTWP11")

  /** @group Constructors */
  case object REQTWS12 extends KeywordName("REQTWS12", "REQTWS12")

  /** @group Constructors */
  case object REQTWD12 extends KeywordName("REQTWD12", "REQTWD12")

  /** @group Constructors */
  case object REQTWN12 extends KeywordName("REQTWN12", "REQTWN12")

  /** @group Constructors */
  case object REQTWP12 extends KeywordName("REQTWP12", "REQTWP12")

  /** @group Constructors */
  case object REQTWS13 extends KeywordName("REQTWS13", "REQTWS13")

  /** @group Constructors */
  case object REQTWD13 extends KeywordName("REQTWD13", "REQTWD13")

  /** @group Constructors */
  case object REQTWN13 extends KeywordName("REQTWN13", "REQTWN13")

  /** @group Constructors */
  case object REQTWP13 extends KeywordName("REQTWP13", "REQTWP13")

  /** @group Constructors */
  case object REQTWS14 extends KeywordName("REQTWS14", "REQTWS14")

  /** @group Constructors */
  case object REQTWD14 extends KeywordName("REQTWD14", "REQTWD14")

  /** @group Constructors */
  case object REQTWN14 extends KeywordName("REQTWN14", "REQTWN14")

  /** @group Constructors */
  case object REQTWP14 extends KeywordName("REQTWP14", "REQTWP14")

  /** @group Constructors */
  case object REQTWS15 extends KeywordName("REQTWS15", "REQTWS15")

  /** @group Constructors */
  case object REQTWD15 extends KeywordName("REQTWD15", "REQTWD15")

  /** @group Constructors */
  case object REQTWN15 extends KeywordName("REQTWN15", "REQTWN15")

  /** @group Constructors */
  case object REQTWP15 extends KeywordName("REQTWP15", "REQTWP15")

  /** @group Constructors */
  case object REQTWS16 extends KeywordName("REQTWS16", "REQTWS16")

  /** @group Constructors */
  case object REQTWD16 extends KeywordName("REQTWD16", "REQTWD16")

  /** @group Constructors */
  case object REQTWN16 extends KeywordName("REQTWN16", "REQTWN16")

  /** @group Constructors */
  case object REQTWP16 extends KeywordName("REQTWP16", "REQTWP16")

  /** @group Constructors */
  case object REQTWS17 extends KeywordName("REQTWS17", "REQTWS17")

  /** @group Constructors */
  case object REQTWD17 extends KeywordName("REQTWD17", "REQTWD17")

  /** @group Constructors */
  case object REQTWN17 extends KeywordName("REQTWN17", "REQTWN17")

  /** @group Constructors */
  case object REQTWP17 extends KeywordName("REQTWP17", "REQTWP17")

  /** @group Constructors */
  case object REQTWS18 extends KeywordName("REQTWS18", "REQTWS18")

  /** @group Constructors */
  case object REQTWD18 extends KeywordName("REQTWD18", "REQTWD18")

  /** @group Constructors */
  case object REQTWN18 extends KeywordName("REQTWN18", "REQTWN18")

  /** @group Constructors */
  case object REQTWP18 extends KeywordName("REQTWP18", "REQTWP18")

  /** @group Constructors */
  case object REQTWS19 extends KeywordName("REQTWS19", "REQTWS19")

  /** @group Constructors */
  case object REQTWD19 extends KeywordName("REQTWD19", "REQTWD19")

  /** @group Constructors */
  case object REQTWN19 extends KeywordName("REQTWN19", "REQTWN19")

  /** @group Constructors */
  case object REQTWP19 extends KeywordName("REQTWP19", "REQTWP19")

  /** @group Constructors */
  case object REQTWS20 extends KeywordName("REQTWS20", "REQTWS20")

  /** @group Constructors */
  case object REQTWD20 extends KeywordName("REQTWD20", "REQTWD20")

  /** @group Constructors */
  case object REQTWN20 extends KeywordName("REQTWN20", "REQTWN20")

  /** @group Constructors */
  case object REQTWP20 extends KeywordName("REQTWP20", "REQTWP20")

  /** @group Constructors */
  case object REQTWS21 extends KeywordName("REQTWS21", "REQTWS21")

  /** @group Constructors */
  case object REQTWD21 extends KeywordName("REQTWD21", "REQTWD21")

  /** @group Constructors */
  case object REQTWN21 extends KeywordName("REQTWN21", "REQTWN21")

  /** @group Constructors */
  case object REQTWP21 extends KeywordName("REQTWP21", "REQTWP21")

  /** @group Constructors */
  case object REQTWS22 extends KeywordName("REQTWS22", "REQTWS22")

  /** @group Constructors */
  case object REQTWD22 extends KeywordName("REQTWD22", "REQTWD22")

  /** @group Constructors */
  case object REQTWN22 extends KeywordName("REQTWN22", "REQTWN22")

  /** @group Constructors */
  case object REQTWP22 extends KeywordName("REQTWP22", "REQTWP22")

  /** @group Constructors */
  case object REQTWS23 extends KeywordName("REQTWS23", "REQTWS23")

  /** @group Constructors */
  case object REQTWD23 extends KeywordName("REQTWD23", "REQTWD23")

  /** @group Constructors */
  case object REQTWN23 extends KeywordName("REQTWN23", "REQTWN23")

  /** @group Constructors */
  case object REQTWP23 extends KeywordName("REQTWP23", "REQTWP23")

  /** @group Constructors */
  case object REQTWS24 extends KeywordName("REQTWS24", "REQTWS24")

  /** @group Constructors */
  case object REQTWD24 extends KeywordName("REQTWD24", "REQTWD24")

  /** @group Constructors */
  case object REQTWN24 extends KeywordName("REQTWN24", "REQTWN24")

  /** @group Constructors */
  case object REQTWP24 extends KeywordName("REQTWP24", "REQTWP24")

  /** @group Constructors */
  case object REQTWS25 extends KeywordName("REQTWS25", "REQTWS25")

  /** @group Constructors */
  case object REQTWD25 extends KeywordName("REQTWD25", "REQTWD25")

  /** @group Constructors */
  case object REQTWN25 extends KeywordName("REQTWN25", "REQTWN25")

  /** @group Constructors */
  case object REQTWP25 extends KeywordName("REQTWP25", "REQTWP25")

  /** @group Constructors */
  case object REQTWS26 extends KeywordName("REQTWS26", "REQTWS26")

  /** @group Constructors */
  case object REQTWD26 extends KeywordName("REQTWD26", "REQTWD26")

  /** @group Constructors */
  case object REQTWN26 extends KeywordName("REQTWN26", "REQTWN26")

  /** @group Constructors */
  case object REQTWP26 extends KeywordName("REQTWP26", "REQTWP26")

  /** @group Constructors */
  case object REQTWS27 extends KeywordName("REQTWS27", "REQTWS27")

  /** @group Constructors */
  case object REQTWD27 extends KeywordName("REQTWD27", "REQTWD27")

  /** @group Constructors */
  case object REQTWN27 extends KeywordName("REQTWN27", "REQTWN27")

  /** @group Constructors */
  case object REQTWP27 extends KeywordName("REQTWP27", "REQTWP27")

  /** @group Constructors */
  case object REQTWS28 extends KeywordName("REQTWS28", "REQTWS28")

  /** @group Constructors */
  case object REQTWD28 extends KeywordName("REQTWD28", "REQTWD28")

  /** @group Constructors */
  case object REQTWN28 extends KeywordName("REQTWN28", "REQTWN28")

  /** @group Constructors */
  case object REQTWP28 extends KeywordName("REQTWP28", "REQTWP28")

  /** @group Constructors */
  case object REQTWS29 extends KeywordName("REQTWS29", "REQTWS29")

  /** @group Constructors */
  case object REQTWD29 extends KeywordName("REQTWD29", "REQTWD29")

  /** @group Constructors */
  case object REQTWN29 extends KeywordName("REQTWN29", "REQTWN29")

  /** @group Constructors */
  case object REQTWP29 extends KeywordName("REQTWP29", "REQTWP29")

  /** @group Constructors */
  case object REQTWS30 extends KeywordName("REQTWS30", "REQTWS30")

  /** @group Constructors */
  case object REQTWD30 extends KeywordName("REQTWD30", "REQTWD30")

  /** @group Constructors */
  case object REQTWN30 extends KeywordName("REQTWN30", "REQTWN30")

  /** @group Constructors */
  case object REQTWP30 extends KeywordName("REQTWP30", "REQTWP30")

  /** @group Constructors */
  case object REQTWS31 extends KeywordName("REQTWS31", "REQTWS31")

  /** @group Constructors */
  case object REQTWD31 extends KeywordName("REQTWD31", "REQTWD31")

  /** @group Constructors */
  case object REQTWN31 extends KeywordName("REQTWN31", "REQTWN31")

  /** @group Constructors */
  case object REQTWP31 extends KeywordName("REQTWP31", "REQTWP31")

  /** @group Constructors */
  case object REQTWS32 extends KeywordName("REQTWS32", "REQTWS32")

  /** @group Constructors */
  case object REQTWD32 extends KeywordName("REQTWD32", "REQTWD32")

  /** @group Constructors */
  case object REQTWN32 extends KeywordName("REQTWN32", "REQTWN32")

  /** @group Constructors */
  case object REQTWP32 extends KeywordName("REQTWP32", "REQTWP32")

  /** @group Constructors */
  case object REQTWS33 extends KeywordName("REQTWS33", "REQTWS33")

  /** @group Constructors */
  case object REQTWD33 extends KeywordName("REQTWD33", "REQTWD33")

  /** @group Constructors */
  case object REQTWN33 extends KeywordName("REQTWN33", "REQTWN33")

  /** @group Constructors */
  case object REQTWP33 extends KeywordName("REQTWP33", "REQTWP33")

  /** @group Constructors */
  case object REQTWS34 extends KeywordName("REQTWS34", "REQTWS34")

  /** @group Constructors */
  case object REQTWD34 extends KeywordName("REQTWD34", "REQTWD34")

  /** @group Constructors */
  case object REQTWN34 extends KeywordName("REQTWN34", "REQTWN34")

  /** @group Constructors */
  case object REQTWP34 extends KeywordName("REQTWP34", "REQTWP34")

  /** @group Constructors */
  case object REQTWS35 extends KeywordName("REQTWS35", "REQTWS35")

  /** @group Constructors */
  case object REQTWD35 extends KeywordName("REQTWD35", "REQTWD35")

  /** @group Constructors */
  case object REQTWN35 extends KeywordName("REQTWN35", "REQTWN35")

  /** @group Constructors */
  case object REQTWP35 extends KeywordName("REQTWP35", "REQTWP35")

  /** @group Constructors */
  case object REQTWS36 extends KeywordName("REQTWS36", "REQTWS36")

  /** @group Constructors */
  case object REQTWD36 extends KeywordName("REQTWD36", "REQTWD36")

  /** @group Constructors */
  case object REQTWN36 extends KeywordName("REQTWN36", "REQTWN36")

  /** @group Constructors */
  case object REQTWP36 extends KeywordName("REQTWP36", "REQTWP36")

  /** @group Constructors */
  case object REQTWS37 extends KeywordName("REQTWS37", "REQTWS37")

  /** @group Constructors */
  case object REQTWD37 extends KeywordName("REQTWD37", "REQTWD37")

  /** @group Constructors */
  case object REQTWN37 extends KeywordName("REQTWN37", "REQTWN37")

  /** @group Constructors */
  case object REQTWP37 extends KeywordName("REQTWP37", "REQTWP37")

  /** @group Constructors */
  case object REQTWS38 extends KeywordName("REQTWS38", "REQTWS38")

  /** @group Constructors */
  case object REQTWD38 extends KeywordName("REQTWD38", "REQTWD38")

  /** @group Constructors */
  case object REQTWN38 extends KeywordName("REQTWN38", "REQTWN38")

  /** @group Constructors */
  case object REQTWP38 extends KeywordName("REQTWP38", "REQTWP38")

  /** @group Constructors */
  case object REQTWS39 extends KeywordName("REQTWS39", "REQTWS39")

  /** @group Constructors */
  case object REQTWD39 extends KeywordName("REQTWD39", "REQTWD39")

  /** @group Constructors */
  case object REQTWN39 extends KeywordName("REQTWN39", "REQTWN39")

  /** @group Constructors */
  case object REQTWP39 extends KeywordName("REQTWP39", "REQTWP39")

  /** @group Constructors */
  case object REQTWS40 extends KeywordName("REQTWS40", "REQTWS40")

  /** @group Constructors */
  case object REQTWD40 extends KeywordName("REQTWD40", "REQTWD40")

  /** @group Constructors */
  case object REQTWN40 extends KeywordName("REQTWN40", "REQTWN40")

  /** @group Constructors */
  case object REQTWP40 extends KeywordName("REQTWP40", "REQTWP40")

  /** @group Constructors */
  case object REQTWS41 extends KeywordName("REQTWS41", "REQTWS41")

  /** @group Constructors */
  case object REQTWD41 extends KeywordName("REQTWD41", "REQTWD41")

  /** @group Constructors */
  case object REQTWN41 extends KeywordName("REQTWN41", "REQTWN41")

  /** @group Constructors */
  case object REQTWP41 extends KeywordName("REQTWP41", "REQTWP41")

  /** @group Constructors */
  case object REQTWS42 extends KeywordName("REQTWS42", "REQTWS42")

  /** @group Constructors */
  case object REQTWD42 extends KeywordName("REQTWD42", "REQTWD42")

  /** @group Constructors */
  case object REQTWN42 extends KeywordName("REQTWN42", "REQTWN42")

  /** @group Constructors */
  case object REQTWP42 extends KeywordName("REQTWP42", "REQTWP42")

  /** @group Constructors */
  case object REQTWS43 extends KeywordName("REQTWS43", "REQTWS43")

  /** @group Constructors */
  case object REQTWD43 extends KeywordName("REQTWD43", "REQTWD43")

  /** @group Constructors */
  case object REQTWN43 extends KeywordName("REQTWN43", "REQTWN43")

  /** @group Constructors */
  case object REQTWP43 extends KeywordName("REQTWP43", "REQTWP43")

  /** @group Constructors */
  case object REQTWS44 extends KeywordName("REQTWS44", "REQTWS44")

  /** @group Constructors */
  case object REQTWD44 extends KeywordName("REQTWD44", "REQTWD44")

  /** @group Constructors */
  case object REQTWN44 extends KeywordName("REQTWN44", "REQTWN44")

  /** @group Constructors */
  case object REQTWP44 extends KeywordName("REQTWP44", "REQTWP44")

  /** @group Constructors */
  case object REQTWS45 extends KeywordName("REQTWS45", "REQTWS45")

  /** @group Constructors */
  case object REQTWD45 extends KeywordName("REQTWD45", "REQTWD45")

  /** @group Constructors */
  case object REQTWN45 extends KeywordName("REQTWN45", "REQTWN45")

  /** @group Constructors */
  case object REQTWP45 extends KeywordName("REQTWP45", "REQTWP45")

  /** @group Constructors */
  case object REQTWS46 extends KeywordName("REQTWS46", "REQTWS46")

  /** @group Constructors */
  case object REQTWD46 extends KeywordName("REQTWD46", "REQTWD46")

  /** @group Constructors */
  case object REQTWN46 extends KeywordName("REQTWN46", "REQTWN46")

  /** @group Constructors */
  case object REQTWP46 extends KeywordName("REQTWP46", "REQTWP46")

  /** @group Constructors */
  case object REQTWS47 extends KeywordName("REQTWS47", "REQTWS47")

  /** @group Constructors */
  case object REQTWD47 extends KeywordName("REQTWD47", "REQTWD47")

  /** @group Constructors */
  case object REQTWN47 extends KeywordName("REQTWN47", "REQTWN47")

  /** @group Constructors */
  case object REQTWP47 extends KeywordName("REQTWP47", "REQTWP47")

  /** @group Constructors */
  case object REQTWS48 extends KeywordName("REQTWS48", "REQTWS48")

  /** @group Constructors */
  case object REQTWD48 extends KeywordName("REQTWD48", "REQTWD48")

  /** @group Constructors */
  case object REQTWN48 extends KeywordName("REQTWN48", "REQTWN48")

  /** @group Constructors */
  case object REQTWP48 extends KeywordName("REQTWP48", "REQTWP48")

  /** @group Constructors */
  case object REQTWS49 extends KeywordName("REQTWS49", "REQTWS49")

  /** @group Constructors */
  case object REQTWD49 extends KeywordName("REQTWD49", "REQTWD49")

  /** @group Constructors */
  case object REQTWN49 extends KeywordName("REQTWN49", "REQTWN49")

  /** @group Constructors */
  case object REQTWP49 extends KeywordName("REQTWP49", "REQTWP49")

  /** @group Constructors */
  case object REQTWS50 extends KeywordName("REQTWS50", "REQTWS50")

  /** @group Constructors */
  case object REQTWD50 extends KeywordName("REQTWD50", "REQTWD50")

  /** @group Constructors */
  case object REQTWN50 extends KeywordName("REQTWN50", "REQTWN50")

  /** @group Constructors */
  case object REQTWP50 extends KeywordName("REQTWP50", "REQTWP50")

  /** @group Constructors */
  case object REQTWS51 extends KeywordName("REQTWS51", "REQTWS51")

  /** @group Constructors */
  case object REQTWD51 extends KeywordName("REQTWD51", "REQTWD51")

  /** @group Constructors */
  case object REQTWN51 extends KeywordName("REQTWN51", "REQTWN51")

  /** @group Constructors */
  case object REQTWP51 extends KeywordName("REQTWP51", "REQTWP51")

  /** @group Constructors */
  case object REQTWS52 extends KeywordName("REQTWS52", "REQTWS52")

  /** @group Constructors */
  case object REQTWD52 extends KeywordName("REQTWD52", "REQTWD52")

  /** @group Constructors */
  case object REQTWN52 extends KeywordName("REQTWN52", "REQTWN52")

  /** @group Constructors */
  case object REQTWP52 extends KeywordName("REQTWP52", "REQTWP52")

  /** @group Constructors */
  case object REQTWS53 extends KeywordName("REQTWS53", "REQTWS53")

  /** @group Constructors */
  case object REQTWD53 extends KeywordName("REQTWD53", "REQTWD53")

  /** @group Constructors */
  case object REQTWN53 extends KeywordName("REQTWN53", "REQTWN53")

  /** @group Constructors */
  case object REQTWP53 extends KeywordName("REQTWP53", "REQTWP53")

  /** @group Constructors */
  case object REQTWS54 extends KeywordName("REQTWS54", "REQTWS54")

  /** @group Constructors */
  case object REQTWD54 extends KeywordName("REQTWD54", "REQTWD54")

  /** @group Constructors */
  case object REQTWN54 extends KeywordName("REQTWN54", "REQTWN54")

  /** @group Constructors */
  case object REQTWP54 extends KeywordName("REQTWP54", "REQTWP54")

  /** @group Constructors */
  case object REQTWS55 extends KeywordName("REQTWS55", "REQTWS55")

  /** @group Constructors */
  case object REQTWD55 extends KeywordName("REQTWD55", "REQTWD55")

  /** @group Constructors */
  case object REQTWN55 extends KeywordName("REQTWN55", "REQTWN55")

  /** @group Constructors */
  case object REQTWP55 extends KeywordName("REQTWP55", "REQTWP55")

  /** @group Constructors */
  case object REQTWS56 extends KeywordName("REQTWS56", "REQTWS56")

  /** @group Constructors */
  case object REQTWD56 extends KeywordName("REQTWD56", "REQTWD56")

  /** @group Constructors */
  case object REQTWN56 extends KeywordName("REQTWN56", "REQTWN56")

  /** @group Constructors */
  case object REQTWP56 extends KeywordName("REQTWP56", "REQTWP56")

  /** @group Constructors */
  case object REQTWS57 extends KeywordName("REQTWS57", "REQTWS57")

  /** @group Constructors */
  case object REQTWD57 extends KeywordName("REQTWD57", "REQTWD57")

  /** @group Constructors */
  case object REQTWN57 extends KeywordName("REQTWN57", "REQTWN57")

  /** @group Constructors */
  case object REQTWP57 extends KeywordName("REQTWP57", "REQTWP57")

  /** @group Constructors */
  case object REQTWS58 extends KeywordName("REQTWS58", "REQTWS58")

  /** @group Constructors */
  case object REQTWD58 extends KeywordName("REQTWD58", "REQTWD58")

  /** @group Constructors */
  case object REQTWN58 extends KeywordName("REQTWN58", "REQTWN58")

  /** @group Constructors */
  case object REQTWP58 extends KeywordName("REQTWP58", "REQTWP58")

  /** @group Constructors */
  case object REQTWS59 extends KeywordName("REQTWS59", "REQTWS59")

  /** @group Constructors */
  case object REQTWD59 extends KeywordName("REQTWD59", "REQTWD59")

  /** @group Constructors */
  case object REQTWN59 extends KeywordName("REQTWN59", "REQTWN59")

  /** @group Constructors */
  case object REQTWP59 extends KeywordName("REQTWP59", "REQTWP59")

  /** @group Constructors */
  case object REQTWS60 extends KeywordName("REQTWS60", "REQTWS60")

  /** @group Constructors */
  case object REQTWD60 extends KeywordName("REQTWD60", "REQTWD60")

  /** @group Constructors */
  case object REQTWN60 extends KeywordName("REQTWN60", "REQTWN60")

  /** @group Constructors */
  case object REQTWP60 extends KeywordName("REQTWP60", "REQTWP60")

  /** @group Constructors */
  case object REQTWS61 extends KeywordName("REQTWS61", "REQTWS61")

  /** @group Constructors */
  case object REQTWD61 extends KeywordName("REQTWD61", "REQTWD61")

  /** @group Constructors */
  case object REQTWN61 extends KeywordName("REQTWN61", "REQTWN61")

  /** @group Constructors */
  case object REQTWP61 extends KeywordName("REQTWP61", "REQTWP61")

  /** @group Constructors */
  case object REQTWS62 extends KeywordName("REQTWS62", "REQTWS62")

  /** @group Constructors */
  case object REQTWD62 extends KeywordName("REQTWD62", "REQTWD62")

  /** @group Constructors */
  case object REQTWN62 extends KeywordName("REQTWN62", "REQTWN62")

  /** @group Constructors */
  case object REQTWP62 extends KeywordName("REQTWP62", "REQTWP62")

  /** @group Constructors */
  case object REQTWS63 extends KeywordName("REQTWS63", "REQTWS63")

  /** @group Constructors */
  case object REQTWD63 extends KeywordName("REQTWD63", "REQTWD63")

  /** @group Constructors */
  case object REQTWN63 extends KeywordName("REQTWN63", "REQTWN63")

  /** @group Constructors */
  case object REQTWP63 extends KeywordName("REQTWP63", "REQTWP63")

  /** @group Constructors */
  case object REQTWS64 extends KeywordName("REQTWS64", "REQTWS64")

  /** @group Constructors */
  case object REQTWD64 extends KeywordName("REQTWD64", "REQTWD64")

  /** @group Constructors */
  case object REQTWN64 extends KeywordName("REQTWN64", "REQTWN64")

  /** @group Constructors */
  case object REQTWP64 extends KeywordName("REQTWP64", "REQTWP64")

  /** @group Constructors */
  case object REQTWS65 extends KeywordName("REQTWS65", "REQTWS65")

  /** @group Constructors */
  case object REQTWD65 extends KeywordName("REQTWD65", "REQTWD65")

  /** @group Constructors */
  case object REQTWN65 extends KeywordName("REQTWN65", "REQTWN65")

  /** @group Constructors */
  case object REQTWP65 extends KeywordName("REQTWP65", "REQTWP65")

  /** @group Constructors */
  case object REQTWS66 extends KeywordName("REQTWS66", "REQTWS66")

  /** @group Constructors */
  case object REQTWD66 extends KeywordName("REQTWD66", "REQTWD66")

  /** @group Constructors */
  case object REQTWN66 extends KeywordName("REQTWN66", "REQTWN66")

  /** @group Constructors */
  case object REQTWP66 extends KeywordName("REQTWP66", "REQTWP66")

  /** @group Constructors */
  case object REQTWS67 extends KeywordName("REQTWS67", "REQTWS67")

  /** @group Constructors */
  case object REQTWD67 extends KeywordName("REQTWD67", "REQTWD67")

  /** @group Constructors */
  case object REQTWN67 extends KeywordName("REQTWN67", "REQTWN67")

  /** @group Constructors */
  case object REQTWP67 extends KeywordName("REQTWP67", "REQTWP67")

  /** @group Constructors */
  case object REQTWS68 extends KeywordName("REQTWS68", "REQTWS68")

  /** @group Constructors */
  case object REQTWD68 extends KeywordName("REQTWD68", "REQTWD68")

  /** @group Constructors */
  case object REQTWN68 extends KeywordName("REQTWN68", "REQTWN68")

  /** @group Constructors */
  case object REQTWP68 extends KeywordName("REQTWP68", "REQTWP68")

  /** @group Constructors */
  case object REQTWS69 extends KeywordName("REQTWS69", "REQTWS69")

  /** @group Constructors */
  case object REQTWD69 extends KeywordName("REQTWD69", "REQTWD69")

  /** @group Constructors */
  case object REQTWN69 extends KeywordName("REQTWN69", "REQTWN69")

  /** @group Constructors */
  case object REQTWP69 extends KeywordName("REQTWP69", "REQTWP69")

  /** @group Constructors */
  case object REQTWS70 extends KeywordName("REQTWS70", "REQTWS70")

  /** @group Constructors */
  case object REQTWD70 extends KeywordName("REQTWD70", "REQTWD70")

  /** @group Constructors */
  case object REQTWN70 extends KeywordName("REQTWN70", "REQTWN70")

  /** @group Constructors */
  case object REQTWP70 extends KeywordName("REQTWP70", "REQTWP70")

  /** @group Constructors */
  case object REQTWS71 extends KeywordName("REQTWS71", "REQTWS71")

  /** @group Constructors */
  case object REQTWD71 extends KeywordName("REQTWD71", "REQTWD71")

  /** @group Constructors */
  case object REQTWN71 extends KeywordName("REQTWN71", "REQTWN71")

  /** @group Constructors */
  case object REQTWP71 extends KeywordName("REQTWP71", "REQTWP71")

  /** @group Constructors */
  case object REQTWS72 extends KeywordName("REQTWS72", "REQTWS72")

  /** @group Constructors */
  case object REQTWD72 extends KeywordName("REQTWD72", "REQTWD72")

  /** @group Constructors */
  case object REQTWN72 extends KeywordName("REQTWN72", "REQTWN72")

  /** @group Constructors */
  case object REQTWP72 extends KeywordName("REQTWP72", "REQTWP72")

  /** @group Constructors */
  case object REQTWS73 extends KeywordName("REQTWS73", "REQTWS73")

  /** @group Constructors */
  case object REQTWD73 extends KeywordName("REQTWD73", "REQTWD73")

  /** @group Constructors */
  case object REQTWN73 extends KeywordName("REQTWN73", "REQTWN73")

  /** @group Constructors */
  case object REQTWP73 extends KeywordName("REQTWP73", "REQTWP73")

  /** @group Constructors */
  case object REQTWS74 extends KeywordName("REQTWS74", "REQTWS74")

  /** @group Constructors */
  case object REQTWD74 extends KeywordName("REQTWD74", "REQTWD74")

  /** @group Constructors */
  case object REQTWN74 extends KeywordName("REQTWN74", "REQTWN74")

  /** @group Constructors */
  case object REQTWP74 extends KeywordName("REQTWP74", "REQTWP74")

  /** @group Constructors */
  case object REQTWS75 extends KeywordName("REQTWS75", "REQTWS75")

  /** @group Constructors */
  case object REQTWD75 extends KeywordName("REQTWD75", "REQTWD75")

  /** @group Constructors */
  case object REQTWN75 extends KeywordName("REQTWN75", "REQTWN75")

  /** @group Constructors */
  case object REQTWP75 extends KeywordName("REQTWP75", "REQTWP75")

  /** @group Constructors */
  case object REQTWS76 extends KeywordName("REQTWS76", "REQTWS76")

  /** @group Constructors */
  case object REQTWD76 extends KeywordName("REQTWD76", "REQTWD76")

  /** @group Constructors */
  case object REQTWN76 extends KeywordName("REQTWN76", "REQTWN76")

  /** @group Constructors */
  case object REQTWP76 extends KeywordName("REQTWP76", "REQTWP76")

  /** @group Constructors */
  case object REQTWS77 extends KeywordName("REQTWS77", "REQTWS77")

  /** @group Constructors */
  case object REQTWD77 extends KeywordName("REQTWD77", "REQTWD77")

  /** @group Constructors */
  case object REQTWN77 extends KeywordName("REQTWN77", "REQTWN77")

  /** @group Constructors */
  case object REQTWP77 extends KeywordName("REQTWP77", "REQTWP77")

  /** @group Constructors */
  case object REQTWS78 extends KeywordName("REQTWS78", "REQTWS78")

  /** @group Constructors */
  case object REQTWD78 extends KeywordName("REQTWD78", "REQTWD78")

  /** @group Constructors */
  case object REQTWN78 extends KeywordName("REQTWN78", "REQTWN78")

  /** @group Constructors */
  case object REQTWP78 extends KeywordName("REQTWP78", "REQTWP78")

  /** @group Constructors */
  case object REQTWS79 extends KeywordName("REQTWS79", "REQTWS79")

  /** @group Constructors */
  case object REQTWD79 extends KeywordName("REQTWD79", "REQTWD79")

  /** @group Constructors */
  case object REQTWN79 extends KeywordName("REQTWN79", "REQTWN79")

  /** @group Constructors */
  case object REQTWP79 extends KeywordName("REQTWP79", "REQTWP79")

  /** @group Constructors */
  case object REQTWS80 extends KeywordName("REQTWS80", "REQTWS80")

  /** @group Constructors */
  case object REQTWD80 extends KeywordName("REQTWD80", "REQTWD80")

  /** @group Constructors */
  case object REQTWN80 extends KeywordName("REQTWN80", "REQTWN80")

  /** @group Constructors */
  case object REQTWP80 extends KeywordName("REQTWP80", "REQTWP80")

  /** @group Constructors */
  case object REQTWS81 extends KeywordName("REQTWS81", "REQTWS81")

  /** @group Constructors */
  case object REQTWD81 extends KeywordName("REQTWD81", "REQTWD81")

  /** @group Constructors */
  case object REQTWN81 extends KeywordName("REQTWN81", "REQTWN81")

  /** @group Constructors */
  case object REQTWP81 extends KeywordName("REQTWP81", "REQTWP81")

  /** @group Constructors */
  case object REQTWS82 extends KeywordName("REQTWS82", "REQTWS82")

  /** @group Constructors */
  case object REQTWD82 extends KeywordName("REQTWD82", "REQTWD82")

  /** @group Constructors */
  case object REQTWN82 extends KeywordName("REQTWN82", "REQTWN82")

  /** @group Constructors */
  case object REQTWP82 extends KeywordName("REQTWP82", "REQTWP82")

  /** @group Constructors */
  case object REQTWS83 extends KeywordName("REQTWS83", "REQTWS83")

  /** @group Constructors */
  case object REQTWD83 extends KeywordName("REQTWD83", "REQTWD83")

  /** @group Constructors */
  case object REQTWN83 extends KeywordName("REQTWN83", "REQTWN83")

  /** @group Constructors */
  case object REQTWP83 extends KeywordName("REQTWP83", "REQTWP83")

  /** @group Constructors */
  case object REQTWS84 extends KeywordName("REQTWS84", "REQTWS84")

  /** @group Constructors */
  case object REQTWD84 extends KeywordName("REQTWD84", "REQTWD84")

  /** @group Constructors */
  case object REQTWN84 extends KeywordName("REQTWN84", "REQTWN84")

  /** @group Constructors */
  case object REQTWP84 extends KeywordName("REQTWP84", "REQTWP84")

  /** @group Constructors */
  case object REQTWS85 extends KeywordName("REQTWS85", "REQTWS85")

  /** @group Constructors */
  case object REQTWD85 extends KeywordName("REQTWD85", "REQTWD85")

  /** @group Constructors */
  case object REQTWN85 extends KeywordName("REQTWN85", "REQTWN85")

  /** @group Constructors */
  case object REQTWP85 extends KeywordName("REQTWP85", "REQTWP85")

  /** @group Constructors */
  case object REQTWS86 extends KeywordName("REQTWS86", "REQTWS86")

  /** @group Constructors */
  case object REQTWD86 extends KeywordName("REQTWD86", "REQTWD86")

  /** @group Constructors */
  case object REQTWN86 extends KeywordName("REQTWN86", "REQTWN86")

  /** @group Constructors */
  case object REQTWP86 extends KeywordName("REQTWP86", "REQTWP86")

  /** @group Constructors */
  case object REQTWS87 extends KeywordName("REQTWS87", "REQTWS87")

  /** @group Constructors */
  case object REQTWD87 extends KeywordName("REQTWD87", "REQTWD87")

  /** @group Constructors */
  case object REQTWN87 extends KeywordName("REQTWN87", "REQTWN87")

  /** @group Constructors */
  case object REQTWP87 extends KeywordName("REQTWP87", "REQTWP87")

  /** @group Constructors */
  case object REQTWS88 extends KeywordName("REQTWS88", "REQTWS88")

  /** @group Constructors */
  case object REQTWD88 extends KeywordName("REQTWD88", "REQTWD88")

  /** @group Constructors */
  case object REQTWN88 extends KeywordName("REQTWN88", "REQTWN88")

  /** @group Constructors */
  case object REQTWP88 extends KeywordName("REQTWP88", "REQTWP88")

  /** @group Constructors */
  case object REQTWS89 extends KeywordName("REQTWS89", "REQTWS89")

  /** @group Constructors */
  case object REQTWD89 extends KeywordName("REQTWD89", "REQTWD89")

  /** @group Constructors */
  case object REQTWN89 extends KeywordName("REQTWN89", "REQTWN89")

  /** @group Constructors */
  case object REQTWP89 extends KeywordName("REQTWP89", "REQTWP89")

  /** @group Constructors */
  case object REQTWS90 extends KeywordName("REQTWS90", "REQTWS90")

  /** @group Constructors */
  case object REQTWD90 extends KeywordName("REQTWD90", "REQTWD90")

  /** @group Constructors */
  case object REQTWN90 extends KeywordName("REQTWN90", "REQTWN90")

  /** @group Constructors */
  case object REQTWP90 extends KeywordName("REQTWP90", "REQTWP90")

  /** @group Constructors */
  case object REQTWS91 extends KeywordName("REQTWS91", "REQTWS91")

  /** @group Constructors */
  case object REQTWD91 extends KeywordName("REQTWD91", "REQTWD91")

  /** @group Constructors */
  case object REQTWN91 extends KeywordName("REQTWN91", "REQTWN91")

  /** @group Constructors */
  case object REQTWP91 extends KeywordName("REQTWP91", "REQTWP91")

  /** @group Constructors */
  case object REQTWS92 extends KeywordName("REQTWS92", "REQTWS92")

  /** @group Constructors */
  case object REQTWD92 extends KeywordName("REQTWD92", "REQTWD92")

  /** @group Constructors */
  case object REQTWN92 extends KeywordName("REQTWN92", "REQTWN92")

  /** @group Constructors */
  case object REQTWP92 extends KeywordName("REQTWP92", "REQTWP92")

  /** @group Constructors */
  case object REQTWS93 extends KeywordName("REQTWS93", "REQTWS93")

  /** @group Constructors */
  case object REQTWD93 extends KeywordName("REQTWD93", "REQTWD93")

  /** @group Constructors */
  case object REQTWN93 extends KeywordName("REQTWN93", "REQTWN93")

  /** @group Constructors */
  case object REQTWP93 extends KeywordName("REQTWP93", "REQTWP93")

  /** @group Constructors */
  case object REQTWS94 extends KeywordName("REQTWS94", "REQTWS94")

  /** @group Constructors */
  case object REQTWD94 extends KeywordName("REQTWD94", "REQTWD94")

  /** @group Constructors */
  case object REQTWN94 extends KeywordName("REQTWN94", "REQTWN94")

  /** @group Constructors */
  case object REQTWP94 extends KeywordName("REQTWP94", "REQTWP94")

  /** @group Constructors */
  case object REQTWS95 extends KeywordName("REQTWS95", "REQTWS95")

  /** @group Constructors */
  case object REQTWD95 extends KeywordName("REQTWD95", "REQTWD95")

  /** @group Constructors */
  case object REQTWN95 extends KeywordName("REQTWN95", "REQTWN95")

  /** @group Constructors */
  case object REQTWP95 extends KeywordName("REQTWP95", "REQTWP95")

  /** @group Constructors */
  case object REQTWS96 extends KeywordName("REQTWS96", "REQTWS96")

  /** @group Constructors */
  case object REQTWD96 extends KeywordName("REQTWD96", "REQTWD96")

  /** @group Constructors */
  case object REQTWN96 extends KeywordName("REQTWN96", "REQTWN96")

  /** @group Constructors */
  case object REQTWP96 extends KeywordName("REQTWP96", "REQTWP96")

  /** @group Constructors */
  case object REQTWS97 extends KeywordName("REQTWS97", "REQTWS97")

  /** @group Constructors */
  case object REQTWD97 extends KeywordName("REQTWD97", "REQTWD97")

  /** @group Constructors */
  case object REQTWN97 extends KeywordName("REQTWN97", "REQTWN97")

  /** @group Constructors */
  case object REQTWP97 extends KeywordName("REQTWP97", "REQTWP97")

  /** @group Constructors */
  case object REQTWS98 extends KeywordName("REQTWS98", "REQTWS98")

  /** @group Constructors */
  case object REQTWD98 extends KeywordName("REQTWD98", "REQTWD98")

  /** @group Constructors */
  case object REQTWN98 extends KeywordName("REQTWN98", "REQTWN98")

  /** @group Constructors */
  case object REQTWP98 extends KeywordName("REQTWP98", "REQTWP98")

  /** @group Constructors */
  case object REQTWS99 extends KeywordName("REQTWS99", "REQTWS99")

  /** @group Constructors */
  case object REQTWD99 extends KeywordName("REQTWD99", "REQTWD99")

  /** @group Constructors */
  case object REQTWN99 extends KeywordName("REQTWN99", "REQTWN99")

  /** @group Constructors */
  case object REQTWP99 extends KeywordName("REQTWP99", "REQTWP99")

  /** @group Constructors */
  case object COADDS extends KeywordName("COADDS", "COADDS")

  /** @group Constructors */
  case object EXPTIME extends KeywordName("EXPTIME", "EXPTIME")

  /** @group Constructors */
  case object FILTER3 extends KeywordName("FILTER3", "FILTER3")

  /** @group Constructors */
  case object FOCUSNAM extends KeywordName("FOCUSNAM", "FOCUSNAM")

  /** @group Constructors */
  case object FOCUSPOS extends KeywordName("FOCUSPOS", "FOCUSPOS")

  /** @group Constructors */
  case object FPMASK extends KeywordName("FPMASK", "FPMASK")

  /** @group Constructors */
  case object BEAMSPLT extends KeywordName("BEAMSPLT", "BEAMSPLT")

  /** @group Constructors */
  case object WINDCOVR extends KeywordName("WINDCOVR", "WINDCOVR")

  /** @group Constructors */
  case object FRMSPCYCL extends KeywordName("FRMSPCYCL", "FRMSPCYC")

  /** @group Constructors */
  case object HDRTIMING extends KeywordName("HDRTIMING", "HDRTIMIN")

  /** @group Constructors */
  case object LNRS extends KeywordName("LNRS", "LNRS")

  /** @group Constructors */
  case object MODE extends KeywordName("MODE", "MODE")

  /** @group Constructors */
  case object NDAVGS extends KeywordName("NDAVGS", "NDAVGS")

  /** @group Constructors */
  case object PVIEW extends KeywordName("PVIEW", "PVIEW")

  /** @group Constructors */
  case object TDETABS extends KeywordName("TDETABS", "TDETABS")

  /** @group Constructors */
  case object TIME extends KeywordName("TIME", "TIME")

  /** @group Constructors */
  case object TMOUNT extends KeywordName("TMOUNT", "TMOUNT")

  /** @group Constructors */
  case object UCODENAM extends KeywordName("UCODENAM", "UCODENAM")

  /** @group Constructors */
  case object UCODETYP extends KeywordName("UCODETYP", "UCODETYP")

  /** @group Constructors */
  case object VDDCL1 extends KeywordName("VDDCL1", "VDDCL1")

  /** @group Constructors */
  case object VDDCL2 extends KeywordName("VDDCL2", "VDDCL2")

  /** @group Constructors */
  case object VDDUC extends KeywordName("VDDUC", "VDDUC")

  /** @group Constructors */
  case object VDET extends KeywordName("VDET", "VDET")

  /** @group Constructors */
  case object VGGCL1 extends KeywordName("VGGCL1", "VGGCL1")

  /** @group Constructors */
  case object VGGCL2 extends KeywordName("VGGCL2", "VGGCL2")

  /** @group Constructors */
  case object VSET extends KeywordName("VSET", "VSET")

  /** @group Constructors */
  case object A_TDETABS extends KeywordName("A_TDETABS", "A_TDETAB")

  /** @group Constructors */
  case object A_TMOUNT extends KeywordName("A_TMOUNT", "A_TMOUNT")

  /** @group Constructors */
  case object A_VDDCL1 extends KeywordName("A_VDDCL1", "A_VDDCL1")

  /** @group Constructors */
  case object A_VDDCL2 extends KeywordName("A_VDDCL2", "A_VDDCL2")

  /** @group Constructors */
  case object A_VDDUC extends KeywordName("A_VDDUC", "A_VDDUC")

  /** @group Constructors */
  case object A_VDET extends KeywordName("A_VDET", "A_VDET")

  /** @group Constructors */
  case object A_VGGCL1 extends KeywordName("A_VGGCL1", "A_VGGCL1")

  /** @group Constructors */
  case object A_VGGCL2 extends KeywordName("A_VGGCL2", "A_VGGCL2")

  /** @group Constructors */
  case object A_VSET extends KeywordName("A_VSET", "A_VSET")

  /** @group Constructors */
  case object APOFFSET extends KeywordName("APOFFSET", "APOFFSET")

  /** @group Constructors */
  case object FLIP extends KeywordName("FLIP", "FLIP")

  /** @group Constructors */
  case object EXPRQ extends KeywordName("EXPRQ", "EXPRQ")

  /** @group Constructors */
  case object DCNAME extends KeywordName("DCNAME", "DCNAME")

  /** @group Constructors */
  case object PERIOD extends KeywordName("PERIOD", "PERIOD")

  /** @group Constructors */
  case object NPERIODS extends KeywordName("NPERIODS", "NPERIODS")

  /** @group Constructors */
  case object EXPMODE extends KeywordName("EXPMODE", "EXPMODE")

  /** @group Constructors */
  case object BIASPWR extends KeywordName("BIASPWR", "BIASPWR")

  /** @group Constructors */
  case object OBSMODE extends KeywordName("OBSMODE", "OBSMODE")

  /** @group Constructors */
  case object RDTIME extends KeywordName("RDTIME", "RDTIME")

  /** @group Constructors */
  case object CTYPE1 extends KeywordName("CTYPE1", "CTYPE1")

  /** @group Constructors */
  case object CRPIX1 extends KeywordName("CRPIX1", "CRPIX1")

  /** @group Constructors */
  case object CRVAL1 extends KeywordName("CRVAL1", "CRVAL1")

  /** @group Constructors */
  case object CTYPE2 extends KeywordName("CTYPE2", "CTYPE2")

  /** @group Constructors */
  case object CRPIX2 extends KeywordName("CRPIX2", "CRPIX2")

  /** @group Constructors */
  case object CRVAL2 extends KeywordName("CRVAL2", "CRVAL2")

  /** @group Constructors */
  case object CD1_1 extends KeywordName("CD1_1", "CD1_1")

  /** @group Constructors */
  case object CD1_2 extends KeywordName("CD1_2", "CD1_2")

  /** @group Constructors */
  case object CD2_1 extends KeywordName("CD2_1", "CD2_1")

  /** @group Constructors */
  case object CD2_2 extends KeywordName("CD2_2", "CD2_2")

  /** @group Constructors */
  case object RADECSYS extends KeywordName("RADECSYS", "RADECSYS")

  /** @group Constructors */
  case object MJD_OBS extends KeywordName("MJD_OBS", "MJD_OBS")

  /** @group Constructors */
  case object APERTURE extends KeywordName("APERTURE", "APERTURE")

  /** @group Constructors */
  case object FILTER extends KeywordName("FILTER", "FILTER")

  /** @group Constructors */
  case object AOFREQ extends KeywordName("AOFREQ", "AOFREQ")

  /** @group Constructors */
  case object AOCOUNTS extends KeywordName("AOCOUNTS", "AOCOUNTS")

  /** @group Constructors */
  case object AOSEEING extends KeywordName("AOSEEING", "AOSEEING")

  /** @group Constructors */
  case object AOWFSX extends KeywordName("AOWFSX", "AOWFSX")

  /** @group Constructors */
  case object AOWFSY extends KeywordName("AOWFSY", "AOWFSY")

  /** @group Constructors */
  case object AOWFSZ extends KeywordName("AOWFSZ", "AOWFSZ")

  /** @group Constructors */
  case object AOGAIN extends KeywordName("AOGAIN", "AOGAIN")

  /** @group Constructors */
  case object AONCPAF extends KeywordName("AONCPAF", "AONCPAF")

  /** @group Constructors */
  case object AONDFILT extends KeywordName("AONDFILT", "AONDFILT")

  /** @group Constructors */
  case object AOFLENS extends KeywordName("AOFLENS", "AOFLENS")

  /** @group Constructors */
  case object AOFLEXF extends KeywordName("AOFLEXF", "AOFLEXF")

  /** @group Constructors */
  case object LGUSTAGE extends KeywordName("LGUSTAGE", "LGUSTAGE")

  /** @group Constructors */
  case object AOBS extends KeywordName("AOBS", "AOBS")

  /** @group Constructors */
  case object LGDFOCUS extends KeywordName("LGDFOCUS", "LGDFOCUS")

  /** @group Constructors */
  case object LGTTCNTS extends KeywordName("LGTTCNTS", "LGTTCNTS")

  /** @group Constructors */
  case object LGTTEXP extends KeywordName("LGTTEXP", "LGTTEXP")

  /** @group Constructors */
  case object LGSFCNTS extends KeywordName("LGSFCNTS", "LGSFCNTS")

  /** @group Constructors */
  case object LGSFEXP extends KeywordName("LGSFEXP", "LGSFEXP")

  /** @group Constructors */
  case object FSMTIP extends KeywordName("FSMTIP", "FSMTIP")

  /** @group Constructors */
  case object FSMTILT extends KeywordName("FSMTILT", "FSMTILT")

  /** @group Constructors */
  case object LGZMPOS extends KeywordName("LGZMPOS", "LGZMPOS")

  /** @group Constructors */
  case object NAALT extends KeywordName("NAALT", "NAALT")

  /** @group Constructors */
  case object NATHICK extends KeywordName("NATHICK", "NATHICK")

  /** @group Constructors */
  case object LGNDFILT extends KeywordName("LGNDFILT", "LGNDFILT")

  /** @group Constructors */
  case object LGTTIRIS extends KeywordName("LGTTIRIS", "LGTTIRIS")

  /** @group Constructors */
  case object ELAPSED extends KeywordName("ELAPSED", "ELAPSED")

  /** @group Constructors */
  case object READDLAY extends KeywordName("READDLAY", "READDLAY")

  /** @group Constructors */
  case object FILT1POS extends KeywordName("FILT1POS", "FILT1POS")

  /** @group Constructors */
  case object FILT1CAR extends KeywordName("FILT1CAR", "FILT1CAR")

  /** @group Constructors */
  case object FILT2POS extends KeywordName("FILT2POS", "FILT2POS")

  /** @group Constructors */
  case object FILT2CAR extends KeywordName("FILT2CAR", "FILT2CAR")

  /** @group Constructors */
  case object UTLWHEEL extends KeywordName("UTLWHEEL", "UTLWHEEL")

  /** @group Constructors */
  case object UTLWPOS extends KeywordName("UTLWPOS", "UTLWPOS")

  /** @group Constructors */
  case object UTLWCAR extends KeywordName("UTLWCAR", "UTLWCAR")

  /** @group Constructors */
  case object CVERPOS extends KeywordName("CVERPOS", "CVERPOS")

  /** @group Constructors */
  case object CVERCAR extends KeywordName("CVERCAR", "CVERCAR")

  /** @group Constructors */
  case object CWSTEMP extends KeywordName("CWSTEMP", "CWSTEMP")

  /** @group Constructors */
  case object DETTEMP extends KeywordName("DETTEMP", "DETTEMP")

  /** @group Constructors */
  case object DETHTEMP extends KeywordName("DETHTEMP", "DETHTEMP")

  /** @group Constructors */
  case object DEWPRES extends KeywordName("DEWPRES", "DEWPRES")

  /** @group Constructors */
  case object RDNOISE extends KeywordName("RDNOISE", "RDNOISE")

  /** @group Constructors */
  case object GAIN extends KeywordName("GAIN", "GAIN")

  /** @group Constructors */
  case object SAMPMODE extends KeywordName("SAMPMODE", "SAMPMODE")

  /** @group Constructors */
  case object NRESETS extends KeywordName("NRESETS", "NRESETS")

  /** @group Constructors */
  case object RSTDLAY extends KeywordName("RSTDLAY", "RSTDLAY")

  /** @group Constructors */
  case object READTIME extends KeywordName("READTIME", "READTIME")

  /** @group Constructors */
  case object BUNIT extends KeywordName("BUNIT", "BUNIT")

  /** @group Constructors */
  case object DCHLTH extends KeywordName("DCHLTH", "DCHLTH")

  /** @group Constructors */
  case object DCSIM extends KeywordName("DCSIM", "DCSIM")

  /** @group Constructors */
  case object DSPTIMBN extends KeywordName("DSPTIMBN", "DSPTIMBN")

  /** @group Constructors */
  case object DSPTIMBV extends KeywordName("DSPTIMBV", "DSPTIMBV")

  /** @group Constructors */
  case object DSPPCIN extends KeywordName("DSPPCIN", "DSPPCIN")

  /** @group Constructors */
  case object DSPPCIV extends KeywordName("DSPPCIV", "DSPPCIV")

  /** @group Constructors */
  case object GSAOI_MJD_OBS extends KeywordName("GSAOI_MJD_OBS", "MJD-OBS")

  /** @group Constructors */
  case object GEMSSADC extends KeywordName("GEMSSADC", "GEMSSADC")

  /** @group Constructors */
  case object GEMSDICH extends KeywordName("GEMSDICH", "GEMSDICH")

  /** @group Constructors */
  case object GEMSASTR extends KeywordName("GEMSASTR", "GEMSASTR")

  /** @group Constructors */
  case object GEMSNADC extends KeywordName("GEMSNADC", "GEMSNADC")

  /** @group Constructors */
  case object LGWFS1CT extends KeywordName("LGWFS1CT", "LGWFS1CT")

  /** @group Constructors */
  case object LGWFS2CT extends KeywordName("LGWFS2CT", "LGWFS2CT")

  /** @group Constructors */
  case object LGWFS3CT extends KeywordName("LGWFS3CT", "LGWFS3CT")

  /** @group Constructors */
  case object LGWFS4CT extends KeywordName("LGWFS4CT", "LGWFS4CT")

  /** @group Constructors */
  case object LGWFS5CT extends KeywordName("LGWFS5CT", "LGWFS5CT")

  /** @group Constructors */
  case object LGSLOOP extends KeywordName("LGSLOOP", "LGSLOOP")

  /** @group Constructors */
  case object TTLOOP extends KeywordName("TTLOOP", "TTLOOP")

  /** @group Constructors */
  case object FOCLOOP extends KeywordName("FOCLOOP", "FOCLOOP")

  /** @group Constructors */
  case object FLEXLOOP extends KeywordName("FLEXLOOP", "FLEXLOOP")

  /** @group Constructors */
  case object LGSSTRHL extends KeywordName("LGSSTRHL", "LGSSTRHL")

  /** @group Constructors */
  case object RZEROVAL extends KeywordName("RZEROVAL", "RZEROVAL")

  /** @group Constructors */
  case object CNSQARE1 extends KeywordName("CNSQARE1", "CNSQARE1")

  /** @group Constructors */
  case object CNSQARE2 extends KeywordName("CNSQARE2", "CNSQARE2")

  /** @group Constructors */
  case object CNSQARE3 extends KeywordName("CNSQARE3", "CNSQARE3")

  /** @group Constructors */
  case object CNSQARE4 extends KeywordName("CNSQARE4", "CNSQARE4")

  /** @group Constructors */
  case object CNSQARE5 extends KeywordName("CNSQARE5", "CNSQARE5")

  /** @group Constructors */
  case object CNSQARE6 extends KeywordName("CNSQARE6", "CNSQARE6")

  /** @group Constructors */
  case object GWFS1CFG extends KeywordName("GWFS1CFG", "GWFS1CFG")

  /** @group Constructors */
  case object GWFS1OBJ extends KeywordName("GWFS1OBJ", "GWFS1OBJ")

  /** @group Constructors */
  case object GWFS1RA extends KeywordName("GWFS1RA", "GWFS1RA")

  /** @group Constructors */
  case object GWFS1DEC extends KeywordName("GWFS1DEC", "GWFS1DEC")

  /** @group Constructors */
  case object GWFS1RV extends KeywordName("GWFS1RV", "GWFS1RV")

  /** @group Constructors */
  case object GWFS1EPC extends KeywordName("GWFS1EPC", "GWFS1EPC")

  /** @group Constructors */
  case object GWFS1EQN extends KeywordName("GWFS1EQN", "GWFS1EQN")

  /** @group Constructors */
  case object GWFS1FRM extends KeywordName("GWFS1FRM", "GWFS1FRM")

  /** @group Constructors */
  case object GWFS1PMD extends KeywordName("GWFS1PMD", "GWFS1PMD")

  /** @group Constructors */
  case object GWFS1PMR extends KeywordName("GWFS1PMR", "GWFS1PMR")

  /** @group Constructors */
  case object GWFS1PAR extends KeywordName("GWFS1PAR", "GWFS1PAR")

  /** @group Constructors */
  case object GWFS1WAV extends KeywordName("GWFS1WAV", "GWFS1WAV")

  /** @group Constructors */
  case object GWFS1X extends KeywordName("GWFS1X", "GWFS1X")

  /** @group Constructors */
  case object GWFS1Y extends KeywordName("GWFS1Y", "GWFS1Y")

  /** @group Constructors */
  case object GWFS1SIZ extends KeywordName("GWFS1SIZ", "GWFS1SIZ")

  /** @group Constructors */
  case object GWFS1CTS extends KeywordName("GWFS1CTS", "GWFS1CTS")

  /** @group Constructors */
  case object GWFS2CFG extends KeywordName("GWFS2CFG", "GWFS2CFG")

  /** @group Constructors */
  case object GWFS2OBJ extends KeywordName("GWFS2OBJ", "GWFS2OBJ")

  /** @group Constructors */
  case object GWFS2RA extends KeywordName("GWFS2RA", "GWFS2RA")

  /** @group Constructors */
  case object GWFS2DEC extends KeywordName("GWFS2DEC", "GWFS2DEC")

  /** @group Constructors */
  case object GWFS2RV extends KeywordName("GWFS2RV", "GWFS2RV")

  /** @group Constructors */
  case object GWFS2EPC extends KeywordName("GWFS2EPC", "GWFS2EPC")

  /** @group Constructors */
  case object GWFS2EQN extends KeywordName("GWFS2EQN", "GWFS2EQN")

  /** @group Constructors */
  case object GWFS2FRM extends KeywordName("GWFS2FRM", "GWFS2FRM")

  /** @group Constructors */
  case object GWFS2PMD extends KeywordName("GWFS2PMD", "GWFS2PMD")

  /** @group Constructors */
  case object GWFS2PMR extends KeywordName("GWFS2PMR", "GWFS2PMR")

  /** @group Constructors */
  case object GWFS2PAR extends KeywordName("GWFS2PAR", "GWFS2PAR")

  /** @group Constructors */
  case object GWFS2WAV extends KeywordName("GWFS2WAV", "GWFS2WAV")

  /** @group Constructors */
  case object GWFS2X extends KeywordName("GWFS2X", "GWFS2X")

  /** @group Constructors */
  case object GWFS2Y extends KeywordName("GWFS2Y", "GWFS2Y")

  /** @group Constructors */
  case object GWFS2SIZ extends KeywordName("GWFS2SIZ", "GWFS2SIZ")

  /** @group Constructors */
  case object GWFS2CTS extends KeywordName("GWFS2CTS", "GWFS2CTS")

  /** @group Constructors */
  case object GWFS3CFG extends KeywordName("GWFS3CFG", "GWFS3CFG")

  /** @group Constructors */
  case object GWFS3OBJ extends KeywordName("GWFS3OBJ", "GWFS3OBJ")

  /** @group Constructors */
  case object GWFS3RA extends KeywordName("GWFS3RA", "GWFS3RA")

  /** @group Constructors */
  case object GWFS3DEC extends KeywordName("GWFS3DEC", "GWFS3DEC")

  /** @group Constructors */
  case object GWFS3RV extends KeywordName("GWFS3RV", "GWFS3RV")

  /** @group Constructors */
  case object GWFS3EPC extends KeywordName("GWFS3EPC", "GWFS3EPC")

  /** @group Constructors */
  case object GWFS3EQN extends KeywordName("GWFS3EQN", "GWFS3EQN")

  /** @group Constructors */
  case object GWFS3FRM extends KeywordName("GWFS3FRM", "GWFS3FRM")

  /** @group Constructors */
  case object GWFS3PMD extends KeywordName("GWFS3PMD", "GWFS3PMD")

  /** @group Constructors */
  case object GWFS3PMR extends KeywordName("GWFS3PMR", "GWFS3PMR")

  /** @group Constructors */
  case object GWFS3PAR extends KeywordName("GWFS3PAR", "GWFS3PAR")

  /** @group Constructors */
  case object GWFS3WAV extends KeywordName("GWFS3WAV", "GWFS3WAV")

  /** @group Constructors */
  case object GWFS3X extends KeywordName("GWFS3X", "GWFS3X")

  /** @group Constructors */
  case object GWFS3Y extends KeywordName("GWFS3Y", "GWFS3Y")

  /** @group Constructors */
  case object GWFS3SIZ extends KeywordName("GWFS3SIZ", "GWFS3SIZ")

  /** @group Constructors */
  case object GWFS3CTS extends KeywordName("GWFS3CTS", "GWFS3CTS")

  /** @group Constructors */
  case object GWFS4CFG extends KeywordName("GWFS4CFG", "GWFS4CFG")

  /** @group Constructors */
  case object GWFS4OBJ extends KeywordName("GWFS4OBJ", "GWFS4OBJ")

  /** @group Constructors */
  case object GWFS4RA extends KeywordName("GWFS4RA", "GWFS4RA")

  /** @group Constructors */
  case object GWFS4DEC extends KeywordName("GWFS4DEC", "GWFS4DEC")

  /** @group Constructors */
  case object GWFS4RV extends KeywordName("GWFS4RV", "GWFS4RV")

  /** @group Constructors */
  case object GWFS4EPC extends KeywordName("GWFS4EPC", "GWFS4EPC")

  /** @group Constructors */
  case object GWFS4EQN extends KeywordName("GWFS4EQN", "GWFS4EQN")

  /** @group Constructors */
  case object GWFS4FRM extends KeywordName("GWFS4FRM", "GWFS4FRM")

  /** @group Constructors */
  case object GWFS4PMD extends KeywordName("GWFS4PMD", "GWFS4PMD")

  /** @group Constructors */
  case object GWFS4PMR extends KeywordName("GWFS4PMR", "GWFS4PMR")

  /** @group Constructors */
  case object GWFS4PAR extends KeywordName("GWFS4PAR", "GWFS4PAR")

  /** @group Constructors */
  case object GWFS4WAV extends KeywordName("GWFS4WAV", "GWFS4WAV")

  /** @group Constructors */
  case object GWFS4X extends KeywordName("GWFS4X", "GWFS4X")

  /** @group Constructors */
  case object GWFS4Y extends KeywordName("GWFS4Y", "GWFS4Y")

  /** @group Constructors */
  case object GWFS4SIZ extends KeywordName("GWFS4SIZ", "GWFS4SIZ")

  /** @group Constructors */
  case object GWFS4CTS extends KeywordName("GWFS4CTS", "GWFS4CTS")

  /** @group Constructors */
  case object NODMODE extends KeywordName("NODMODE", "NODMODE")

  /** @group Constructors */
  case object NODPIX extends KeywordName("NODPIX", "NODPIX")

  /** @group Constructors */
  case object NODCOUNT extends KeywordName("NODCOUNT", "NODCOUNT")

  /** @group Constructors */
  case object NODAXOFF extends KeywordName("NODAXOFF", "NODAXOFF")

  /** @group Constructors */
  case object NODAYOFF extends KeywordName("NODAYOFF", "NODAYOFF")

  /** @group Constructors */
  case object NODBXOFF extends KeywordName("NODBXOFF", "NODBXOFF")

  /** @group Constructors */
  case object NODBYOFF extends KeywordName("NODBYOFF", "NODBYOFF")

  /** @group Constructors */
  case object ANODCNT extends KeywordName("ANODCNT", "ANODCNT")

  /** @group Constructors */
  case object BNODCNT extends KeywordName("BNODCNT", "BNODCNT")

  /** @group Constructors */
  case object SUBINT extends KeywordName("SUBINT", "SUBINT")

  /** @group Constructors */
  case object BASEPO extends KeywordName("BASEPO", "BASEPO")

  /** @group Constructors */
  case object SRIFU1 extends KeywordName("SRIFU1", "SRIFU1")

  /** @group Constructors */
  case object SRIFU2 extends KeywordName("SRIFU2", "SRIFU2")

  /** @group Constructors */
  case object HRIFU1 extends KeywordName("HRIFU1", "HRIFU1")

  /** @group Constructors */
  case object HRIFU2 extends KeywordName("HRIFU2", "HRIFU2")

  /** @group Constructors */
  case object IFU1GUID extends KeywordName("IFU1GUID", "IFU1GUID")

  /** @group Constructors */
  case object IFU2GUID extends KeywordName("IFU2GUID", "IFU2GUID")

  /** @group Constructors */
  case object FAGITAT1 extends KeywordName("FAGITAT1", "FAGITAT1")

  /** @group Constructors */
  case object FAGITAT2 extends KeywordName("FAGITAT2", "FAGITAT2")

  /** @group Constructors */
  case object NREDEXP extends KeywordName("NREDEXP", "NREDEXP")

  /** @group Constructors */
  case object REDEXPT extends KeywordName("REDEXPT", "REDEXPT")

  /** @group Constructors */
  case object NBLUEEXP extends KeywordName("NBLUEEXP", "NBLUEEXP")

  /** @group Constructors */
  case object BLUEEXPT extends KeywordName("BLUEEXPT", "BLUEEXPT")

  /** @group Constructors */
  case object NSLITEXP extends KeywordName("NSLITEXP", "NSLITEXP")

  /** @group Constructors */
  case object SLITEXPT extends KeywordName("SLITEXPT", "SLITEXPT")

  /** @group Constructors */
  case object REDCCDS extends KeywordName("REDCCDS", "REDCCDS")

  /** @group Constructors */
  case object BLUCCDS extends KeywordName("BLUCCDS", "BLUCCDS")

  /** @group Constructors */
  case object READRED extends KeywordName("READRED", "READRED")

  /** @group Constructors */
  case object READBLU extends KeywordName("READBLU", "READBLU")

  /** @group Constructors */
  case object TARGETM extends KeywordName("TARGETM", "TARGETM")

  /** @group Constructors */
  case object RESOLUT extends KeywordName("RESOLUT", "RESOLUT")

  /** @group Constructors */
  case object TEXPTIME extends KeywordName("TEXPTIME", "TEXPTIME")

  /** @group Constructors */
  case object TARGET1 extends KeywordName("TARGET1", "TARGET1")

  /** @group Constructors */
  case object TARGET2 extends KeywordName("TARGET2", "TARGET2")

  /** @group Constructors */
  case object HAEND extends KeywordName("HAEND", "HAEND")

  /** @group Constructors */
  case object DATEEND extends KeywordName("DATEEND", "DATE_END")

  /** All members of KeywordName, in canonical order. */
  val all: List[KeywordName] =
    List(
      INSTRUMENT,
      SEQEXVER,
      OBJECT,
      OBSTYPE,
      OBSCLASS,
      GEMPRGID,
      OBSID,
      DATALAB,
      OBSERVER,
      OBSERVAT,
      TELESCOP,
      PARALLAX,
      RADVEL,
      EPOCH,
      EQUINOX,
      TRKEQUIN,
      SSA,
      RA,
      DEC,
      ELEVATIO,
      AZIMUTH,
      CRPA,
      HA,
      LT,
      TRKFRAME,
      DECTRACK,
      TRKEPOCH,
      RATRACK,
      FRAME,
      PMDEC,
      PMRA,
      WAVELENG,
      RAWIQ,
      RAWCC,
      RAWWV,
      RAWBG,
      RAWPIREQ,
      RAWGEMQA,
      CGUIDMOD,
      UT,
      DATE,
      M2BAFFLE,
      M2CENBAF,
      ST,
      XOFFSET,
      YOFFSET,
      POFFSET,
      QOFFSET,
      RAOFFSET,
      DECOFFSE,
      RATRGOFF,
      DECTRGOF,
      PA,
      IAA,
      SFRT2,
      SFTILT,
      SFLINEAR,
      AOFOLD,
      PWFS1_ST,
      PWFS2_ST,
      OIWFS_ST,
      AOWFS_ST,
      SCIBAND,
      NUMREQTW,
      REQIQ,
      REQCC,
      REQBG,
      REQWV,
      REQMAXAM,
      REQMAXHA,
      REQMINAM,
      REQMINHA,
      OIARA,
      OIADEC,
      OIARV,
      OIAWAVEL,
      OIAEPOCH,
      OIAEQUIN,
      OIAFRAME,
      OIAOBJEC,
      OIAPMDEC,
      OIAPMRA,
      OIAPARAL,
      OIFOCUS,
      P1ARA,
      P1ADEC,
      P1ARV,
      P1AWAVEL,
      P1AEPOCH,
      P1AEQUIN,
      P1AFRAME,
      P1AOBJEC,
      P1APMDEC,
      P1APMRA,
      P1APARAL,
      P1FOCUS,
      P2ARA,
      P2ADEC,
      P2ARV,
      P2AWAVEL,
      P2AEPOCH,
      P2AEQUIN,
      P2AFRAME,
      P2AOBJEC,
      P2APMDEC,
      P2APMRA,
      P2APARAL,
      P2FOCUS,
      AOARA,
      AOADEC,
      AOARV,
      AOAWAVEL,
      AOAEPOCH,
      AOAEQUIN,
      AOAFRAME,
      AOAOBJEC,
      AOAPMDEC,
      AOAPMRA,
      AOAPARAL,
      AOFOCUS,
      OIFREQ,
      P1FREQ,
      P2FREQ,
      AIRMASS,
      AMSTART,
      AMEND,
      PROP_MD,
      RELEASE,
      PREIMAGE,
      DATE_OBS,
      TIME_OBS,
      READMODE,
      NREADS,
      GCALLAMP,
      GCALFILT,
      GCALDIFF,
      GCALSHUT,
      PAR_ANG,
      INPORT,
      ASTROMTC,
      CRFOLLOW,
      HUMIDITY,
      TAMBIENT,
      TAMBIEN2,
      PRESSURE,
      PRESSUR2,
      DEWPOINT,
      DEWPOIN2,
      WINDSPEE,
      WINDSPE2,
      WINDDIRE,
      ARRAYID,
      ARRAYTYP,
      UTSTART,
      FILTER1,
      FW1_ENG,
      FILTER2,
      FW2_ENG,
      CAMERA,
      CAM_ENG,
      SLIT,
      SLIT_ENG,
      DECKER,
      DKR_ENG,
      GRATING,
      GR_ENG,
      GRATWAVE,
      GRATORD,
      GRATTILT,
      PRISM,
      PRSM_ENG,
      ACQMIR,
      COVER,
      FOCUS,
      FCS_ENG,
      DETBIAS,
      UTEND,
      OBSEPOCH,
      GMOSCC,
      ADCENPST,
      ADCENPEN,
      ADCENPME,
      ADCEXPST,
      ADCEXPEN,
      ADCEXPME,
      ADCWLEN1,
      ADCWLEN2,
      MASKID,
      MASKNAME,
      MASKTYP,
      MASKLOC,
      FILTID1,
      FILTID2,
      GRATID,
      GRWLEN,
      CENTWAVE,
      GRORDER,
      GRTILT,
      GRSTEP,
      DTAX,
      DTAY,
      DTAZ,
      DTAZST,
      DTAZEN,
      DTAZME,
      DTMODE,
      ADCMODE,
      GMOSDC,
      DETTYPE,
      DETID,
      EXPOSURE,
      ADCUSED,
      DETNROI,
      DETRO0X,
      DETRO0XS,
      DETRO0Y,
      DETRO0YS,
      DETRO1X,
      DETRO1XS,
      DETRO1Y,
      DETRO1YS,
      DETRO2X,
      DETRO2XS,
      DETRO2Y,
      DETRO2YS,
      DETRO3X,
      DETRO3XS,
      DETRO3Y,
      DETRO3YS,
      DETRO4X,
      DETRO4XS,
      DETRO4Y,
      DETRO4YS,
      DETRO5X,
      DETRO5XS,
      DETRO5Y,
      DETRO5YS,
      DETRO6X,
      DETRO6XS,
      DETRO6Y,
      DETRO6YS,
      DETRO7X,
      DETRO7XS,
      DETRO7Y,
      DETRO7YS,
      DETRO8X,
      DETRO8XS,
      DETRO8Y,
      DETRO8YS,
      DETRO9X,
      DETRO9XS,
      DETRO9Y,
      DETRO9YS,
      REQTWS01,
      REQTWD01,
      REQTWN01,
      REQTWP01,
      REQTWS02,
      REQTWD02,
      REQTWN02,
      REQTWP02,
      REQTWS03,
      REQTWD03,
      REQTWN03,
      REQTWP03,
      REQTWS04,
      REQTWD04,
      REQTWN04,
      REQTWP04,
      REQTWS05,
      REQTWD05,
      REQTWN05,
      REQTWP05,
      REQTWS06,
      REQTWD06,
      REQTWN06,
      REQTWP06,
      REQTWS07,
      REQTWD07,
      REQTWN07,
      REQTWP07,
      REQTWS08,
      REQTWD08,
      REQTWN08,
      REQTWP08,
      REQTWS09,
      REQTWD09,
      REQTWN09,
      REQTWP09,
      REQTWS10,
      REQTWD10,
      REQTWN10,
      REQTWP10,
      REQTWS11,
      REQTWD11,
      REQTWN11,
      REQTWP11,
      REQTWS12,
      REQTWD12,
      REQTWN12,
      REQTWP12,
      REQTWS13,
      REQTWD13,
      REQTWN13,
      REQTWP13,
      REQTWS14,
      REQTWD14,
      REQTWN14,
      REQTWP14,
      REQTWS15,
      REQTWD15,
      REQTWN15,
      REQTWP15,
      REQTWS16,
      REQTWD16,
      REQTWN16,
      REQTWP16,
      REQTWS17,
      REQTWD17,
      REQTWN17,
      REQTWP17,
      REQTWS18,
      REQTWD18,
      REQTWN18,
      REQTWP18,
      REQTWS19,
      REQTWD19,
      REQTWN19,
      REQTWP19,
      REQTWS20,
      REQTWD20,
      REQTWN20,
      REQTWP20,
      REQTWS21,
      REQTWD21,
      REQTWN21,
      REQTWP21,
      REQTWS22,
      REQTWD22,
      REQTWN22,
      REQTWP22,
      REQTWS23,
      REQTWD23,
      REQTWN23,
      REQTWP23,
      REQTWS24,
      REQTWD24,
      REQTWN24,
      REQTWP24,
      REQTWS25,
      REQTWD25,
      REQTWN25,
      REQTWP25,
      REQTWS26,
      REQTWD26,
      REQTWN26,
      REQTWP26,
      REQTWS27,
      REQTWD27,
      REQTWN27,
      REQTWP27,
      REQTWS28,
      REQTWD28,
      REQTWN28,
      REQTWP28,
      REQTWS29,
      REQTWD29,
      REQTWN29,
      REQTWP29,
      REQTWS30,
      REQTWD30,
      REQTWN30,
      REQTWP30,
      REQTWS31,
      REQTWD31,
      REQTWN31,
      REQTWP31,
      REQTWS32,
      REQTWD32,
      REQTWN32,
      REQTWP32,
      REQTWS33,
      REQTWD33,
      REQTWN33,
      REQTWP33,
      REQTWS34,
      REQTWD34,
      REQTWN34,
      REQTWP34,
      REQTWS35,
      REQTWD35,
      REQTWN35,
      REQTWP35,
      REQTWS36,
      REQTWD36,
      REQTWN36,
      REQTWP36,
      REQTWS37,
      REQTWD37,
      REQTWN37,
      REQTWP37,
      REQTWS38,
      REQTWD38,
      REQTWN38,
      REQTWP38,
      REQTWS39,
      REQTWD39,
      REQTWN39,
      REQTWP39,
      REQTWS40,
      REQTWD40,
      REQTWN40,
      REQTWP40,
      REQTWS41,
      REQTWD41,
      REQTWN41,
      REQTWP41,
      REQTWS42,
      REQTWD42,
      REQTWN42,
      REQTWP42,
      REQTWS43,
      REQTWD43,
      REQTWN43,
      REQTWP43,
      REQTWS44,
      REQTWD44,
      REQTWN44,
      REQTWP44,
      REQTWS45,
      REQTWD45,
      REQTWN45,
      REQTWP45,
      REQTWS46,
      REQTWD46,
      REQTWN46,
      REQTWP46,
      REQTWS47,
      REQTWD47,
      REQTWN47,
      REQTWP47,
      REQTWS48,
      REQTWD48,
      REQTWN48,
      REQTWP48,
      REQTWS49,
      REQTWD49,
      REQTWN49,
      REQTWP49,
      REQTWS50,
      REQTWD50,
      REQTWN50,
      REQTWP50,
      REQTWS51,
      REQTWD51,
      REQTWN51,
      REQTWP51,
      REQTWS52,
      REQTWD52,
      REQTWN52,
      REQTWP52,
      REQTWS53,
      REQTWD53,
      REQTWN53,
      REQTWP53,
      REQTWS54,
      REQTWD54,
      REQTWN54,
      REQTWP54,
      REQTWS55,
      REQTWD55,
      REQTWN55,
      REQTWP55,
      REQTWS56,
      REQTWD56,
      REQTWN56,
      REQTWP56,
      REQTWS57,
      REQTWD57,
      REQTWN57,
      REQTWP57,
      REQTWS58,
      REQTWD58,
      REQTWN58,
      REQTWP58,
      REQTWS59,
      REQTWD59,
      REQTWN59,
      REQTWP59,
      REQTWS60,
      REQTWD60,
      REQTWN60,
      REQTWP60,
      REQTWS61,
      REQTWD61,
      REQTWN61,
      REQTWP61,
      REQTWS62,
      REQTWD62,
      REQTWN62,
      REQTWP62,
      REQTWS63,
      REQTWD63,
      REQTWN63,
      REQTWP63,
      REQTWS64,
      REQTWD64,
      REQTWN64,
      REQTWP64,
      REQTWS65,
      REQTWD65,
      REQTWN65,
      REQTWP65,
      REQTWS66,
      REQTWD66,
      REQTWN66,
      REQTWP66,
      REQTWS67,
      REQTWD67,
      REQTWN67,
      REQTWP67,
      REQTWS68,
      REQTWD68,
      REQTWN68,
      REQTWP68,
      REQTWS69,
      REQTWD69,
      REQTWN69,
      REQTWP69,
      REQTWS70,
      REQTWD70,
      REQTWN70,
      REQTWP70,
      REQTWS71,
      REQTWD71,
      REQTWN71,
      REQTWP71,
      REQTWS72,
      REQTWD72,
      REQTWN72,
      REQTWP72,
      REQTWS73,
      REQTWD73,
      REQTWN73,
      REQTWP73,
      REQTWS74,
      REQTWD74,
      REQTWN74,
      REQTWP74,
      REQTWS75,
      REQTWD75,
      REQTWN75,
      REQTWP75,
      REQTWS76,
      REQTWD76,
      REQTWN76,
      REQTWP76,
      REQTWS77,
      REQTWD77,
      REQTWN77,
      REQTWP77,
      REQTWS78,
      REQTWD78,
      REQTWN78,
      REQTWP78,
      REQTWS79,
      REQTWD79,
      REQTWN79,
      REQTWP79,
      REQTWS80,
      REQTWD80,
      REQTWN80,
      REQTWP80,
      REQTWS81,
      REQTWD81,
      REQTWN81,
      REQTWP81,
      REQTWS82,
      REQTWD82,
      REQTWN82,
      REQTWP82,
      REQTWS83,
      REQTWD83,
      REQTWN83,
      REQTWP83,
      REQTWS84,
      REQTWD84,
      REQTWN84,
      REQTWP84,
      REQTWS85,
      REQTWD85,
      REQTWN85,
      REQTWP85,
      REQTWS86,
      REQTWD86,
      REQTWN86,
      REQTWP86,
      REQTWS87,
      REQTWD87,
      REQTWN87,
      REQTWP87,
      REQTWS88,
      REQTWD88,
      REQTWN88,
      REQTWP88,
      REQTWS89,
      REQTWD89,
      REQTWN89,
      REQTWP89,
      REQTWS90,
      REQTWD90,
      REQTWN90,
      REQTWP90,
      REQTWS91,
      REQTWD91,
      REQTWN91,
      REQTWP91,
      REQTWS92,
      REQTWD92,
      REQTWN92,
      REQTWP92,
      REQTWS93,
      REQTWD93,
      REQTWN93,
      REQTWP93,
      REQTWS94,
      REQTWD94,
      REQTWN94,
      REQTWP94,
      REQTWS95,
      REQTWD95,
      REQTWN95,
      REQTWP95,
      REQTWS96,
      REQTWD96,
      REQTWN96,
      REQTWP96,
      REQTWS97,
      REQTWD97,
      REQTWN97,
      REQTWP97,
      REQTWS98,
      REQTWD98,
      REQTWN98,
      REQTWP98,
      REQTWS99,
      REQTWD99,
      REQTWN99,
      REQTWP99,
      COADDS,
      EXPTIME,
      FILTER3,
      FOCUSNAM,
      FOCUSPOS,
      FPMASK,
      BEAMSPLT,
      WINDCOVR,
      FRMSPCYCL,
      HDRTIMING,
      LNRS,
      MODE,
      NDAVGS,
      PVIEW,
      TDETABS,
      TIME,
      TMOUNT,
      UCODENAM,
      UCODETYP,
      VDDCL1,
      VDDCL2,
      VDDUC,
      VDET,
      VGGCL1,
      VGGCL2,
      VSET,
      A_TDETABS,
      A_TMOUNT,
      A_VDDCL1,
      A_VDDCL2,
      A_VDDUC,
      A_VDET,
      A_VGGCL1,
      A_VGGCL2,
      A_VSET,
      APOFFSET,
      FLIP,
      EXPRQ,
      DCNAME,
      PERIOD,
      NPERIODS,
      EXPMODE,
      BIASPWR,
      OBSMODE,
      RDTIME,
      CTYPE1,
      CRPIX1,
      CRVAL1,
      CTYPE2,
      CRPIX2,
      CRVAL2,
      CD1_1,
      CD1_2,
      CD2_1,
      CD2_2,
      RADECSYS,
      MJD_OBS,
      APERTURE,
      FILTER,
      AOFREQ,
      AOCOUNTS,
      AOSEEING,
      AOWFSX,
      AOWFSY,
      AOWFSZ,
      AOGAIN,
      AONCPAF,
      AONDFILT,
      AOFLENS,
      AOFLEXF,
      LGUSTAGE,
      AOBS,
      LGDFOCUS,
      LGTTCNTS,
      LGTTEXP,
      LGSFCNTS,
      LGSFEXP,
      FSMTIP,
      FSMTILT,
      LGZMPOS,
      NAALT,
      NATHICK,
      LGNDFILT,
      LGTTIRIS,
      ELAPSED,
      READDLAY,
      FILT1POS,
      FILT1CAR,
      FILT2POS,
      FILT2CAR,
      UTLWHEEL,
      UTLWPOS,
      UTLWCAR,
      CVERPOS,
      CVERCAR,
      CWSTEMP,
      DETTEMP,
      DETHTEMP,
      DEWPRES,
      RDNOISE,
      GAIN,
      SAMPMODE,
      NRESETS,
      RSTDLAY,
      READTIME,
      BUNIT,
      DCHLTH,
      DCSIM,
      DSPTIMBN,
      DSPTIMBV,
      DSPPCIN,
      DSPPCIV,
      GSAOI_MJD_OBS,
      GEMSSADC,
      GEMSDICH,
      GEMSASTR,
      GEMSNADC,
      LGWFS1CT,
      LGWFS2CT,
      LGWFS3CT,
      LGWFS4CT,
      LGWFS5CT,
      LGSLOOP,
      TTLOOP,
      FOCLOOP,
      FLEXLOOP,
      LGSSTRHL,
      RZEROVAL,
      CNSQARE1,
      CNSQARE2,
      CNSQARE3,
      CNSQARE4,
      CNSQARE5,
      CNSQARE6,
      GWFS1CFG,
      GWFS1OBJ,
      GWFS1RA,
      GWFS1DEC,
      GWFS1RV,
      GWFS1EPC,
      GWFS1EQN,
      GWFS1FRM,
      GWFS1PMD,
      GWFS1PMR,
      GWFS1PAR,
      GWFS1WAV,
      GWFS1X,
      GWFS1Y,
      GWFS1SIZ,
      GWFS1CTS,
      GWFS2CFG,
      GWFS2OBJ,
      GWFS2RA,
      GWFS2DEC,
      GWFS2RV,
      GWFS2EPC,
      GWFS2EQN,
      GWFS2FRM,
      GWFS2PMD,
      GWFS2PMR,
      GWFS2PAR,
      GWFS2WAV,
      GWFS2X,
      GWFS2Y,
      GWFS2SIZ,
      GWFS2CTS,
      GWFS3CFG,
      GWFS3OBJ,
      GWFS3RA,
      GWFS3DEC,
      GWFS3RV,
      GWFS3EPC,
      GWFS3EQN,
      GWFS3FRM,
      GWFS3PMD,
      GWFS3PMR,
      GWFS3PAR,
      GWFS3WAV,
      GWFS3X,
      GWFS3Y,
      GWFS3SIZ,
      GWFS3CTS,
      GWFS4CFG,
      GWFS4OBJ,
      GWFS4RA,
      GWFS4DEC,
      GWFS4RV,
      GWFS4EPC,
      GWFS4EQN,
      GWFS4FRM,
      GWFS4PMD,
      GWFS4PMR,
      GWFS4PAR,
      GWFS4WAV,
      GWFS4X,
      GWFS4Y,
      GWFS4SIZ,
      GWFS4CTS,
      NODMODE,
      NODPIX,
      NODCOUNT,
      NODAXOFF,
      NODAYOFF,
      NODBXOFF,
      NODBYOFF,
      ANODCNT,
      BNODCNT,
      SUBINT,
      BASEPO,
      SRIFU1,
      SRIFU2,
      HRIFU1,
      HRIFU2,
      IFU1GUID,
      IFU2GUID,
      FAGITAT1,
      FAGITAT2,
      NREDEXP,
      REDEXPT,
      NBLUEEXP,
      BLUEEXPT,
      NSLITEXP,
      SLITEXPT,
      REDCCDS,
      BLUCCDS,
      READRED,
      READBLU,
      TARGETM,
      RESOLUT,
      TEXPTIME,
      TARGET1,
      TARGET2
    )

  /** Select the member of KeywordName with the given tag, if any. */
  def fromTag(s: String): Option[KeywordName] =
    all.find(_.tag === s)

  /** Select the member of KeywordName with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): KeywordName =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"KeywordName: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val KeywordNameEnumerated: Enumerated[KeywordName] =
    new Enumerated[KeywordName] {
      def all = KeywordName.all
      def tag(a: KeywordName) = a.tag
      override def unsafeFromTag(s: String): KeywordName =
        KeywordName.unsafeFromTag(s)
    }

}
