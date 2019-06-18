// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.arb.{ ArbEnumerated, ArbGmos }
import gem.enum.GmosNorthDisperser
import gem.instances.time._
import gem.config.GmosConfig._
import gsp.math.arb.{ ArbOffset, ArbTime, ArbWavelength }
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import monocle.law.discipline._

final class GmosConfigSpec extends CatsSuite {

  import ArbEnumerated._
  import ArbGmos._
  import ArbOffset._
  import ArbTime._
  import ArbWavelength._

  // Laws

  checkAll("StaticConfig.GmosN", EqTests[StaticConfig.GmosN].eqv)

  checkAll("StaticConfig.GmosN.common",        LensTests(StaticConfig.GmosN.common))
  checkAll("StaticConfig.GmosN.stageMode",     LensTests(StaticConfig.GmosN.stageMode))
  checkAll("StaticConfig.GmosN.customRois",    LensTests(StaticConfig.GmosN.customRois))
  checkAll("StaticConfig.GmosN.nodAndShuffle", LensTests(StaticConfig.GmosN.nodAndShuffle))

  checkAll("StaticConfig.GmosS", EqTests[StaticConfig.GmosS].eqv)

  checkAll("StaticConfig.GmosS.common",        LensTests(StaticConfig.GmosS.common))
  checkAll("StaticConfig.GmosS.stageMode",     LensTests(StaticConfig.GmosS.stageMode))
  checkAll("StaticConfig.GmosS.customRois",    LensTests(StaticConfig.GmosS.customRois))
  checkAll("StaticConfig.GmosS.nodAndShuffle", LensTests(StaticConfig.GmosS.nodAndShuffle))


  checkAll("DynamicConfig.GmosN", EqTests[DynamicConfig.GmosN].eqv)

  checkAll("DynamicConfig.GmosN.common",  LensTests(DynamicConfig.GmosN.common ))
  checkAll("DynamicConfig.GmosN.grating", LensTests(DynamicConfig.GmosN.grating))
  checkAll("DynamicConfig.GmosN.filter",  LensTests(DynamicConfig.GmosN.filter))
  checkAll("DynamicConfig.GmosN.fpu",     LensTests(DynamicConfig.GmosN.fpu))

  checkAll("DynamicConfig.GmosN.xBinning",     LensTests(DynamicConfig.GmosN.xBinning))
  checkAll("DynamicConfig.GmosN.yBinning",     LensTests(DynamicConfig.GmosN.yBinning))
  checkAll("DynamicConfig.GmosN.exposureTime", LensTests(DynamicConfig.GmosN.exposureTime))
  checkAll("DynamicConfig.GmosN.roi",          LensTests(DynamicConfig.GmosN.roi))

  checkAll("DynamicConfig.GmosN.disperser",  OptionalTests(DynamicConfig.GmosN.disperser))
  checkAll("DynamicConfig.GmosN.wavelength", OptionalTests(DynamicConfig.GmosN.wavelength))
  checkAll("DynamicConfig.GmosN.builtinFpu", OptionalTests(DynamicConfig.GmosN.builtinFpu))
  checkAll("DynamicConfig.GmosN.customMask", OptionalTests(DynamicConfig.GmosN.customMask))

  checkAll("DynamicConfig.GmosS", EqTests[DynamicConfig.GmosS].eqv)

  checkAll("DynamicConfig.GmosS.common",  LensTests(DynamicConfig.GmosS.common ))
  checkAll("DynamicConfig.GmosS.grating", LensTests(DynamicConfig.GmosS.grating))
  checkAll("DynamicConfig.GmosS.filter",  LensTests(DynamicConfig.GmosS.filter))
  checkAll("DynamicConfig.GmosS.fpu",     LensTests(DynamicConfig.GmosS.fpu))

  checkAll("DynamicConfig.GmosS.xBinning",     LensTests(DynamicConfig.GmosS.xBinning))
  checkAll("DynamicConfig.GmosS.yBinning",     LensTests(DynamicConfig.GmosS.yBinning))
  checkAll("DynamicConfig.GmosS.exposureTime", LensTests(DynamicConfig.GmosS.exposureTime))
  checkAll("DynamicConfig.GmosS.roi",          LensTests(DynamicConfig.GmosS.roi))

  checkAll("DynamicConfig.GmosS.disperser",  OptionalTests(DynamicConfig.GmosS.disperser))
  checkAll("DynamicConfig.GmosS.wavelength", OptionalTests(DynamicConfig.GmosS.wavelength))
  checkAll("DynamicConfig.GmosS.builtinFpu", OptionalTests(DynamicConfig.GmosS.builtinFpu))
  checkAll("DynamicConfig.GmosS.customMask", OptionalTests(DynamicConfig.GmosS.customMask))

  checkAll("GmosCcdReadout",          EqTests[GmosCcdReadout].eqv)
  checkAll("GmosCommonDynamicConfig", EqTests[GmosCommonDynamicConfig].eqv)
  checkAll("GmosCommonStaticConfig",  EqTests[GmosCommonStaticConfig].eqv)
  checkAll("GmosCustomMask",          EqTests[GmosCustomMask].eqv)
  checkAll("GmosCustomRoiEntry",      OrderTests[GmosCustomRoiEntry].order)
  checkAll("GmosGrating",             EqTests[GmosGrating[GmosNorthDisperser]].eqv)

  checkAll("GmosNodAndShuffle.posA",    LensTests(GmosNodAndShuffle.posA))
  checkAll("GmosNodAndShuffle.posB",    LensTests(GmosNodAndShuffle.posB))
  checkAll("GmosNodAndShuffle.eOffset", LensTests(GmosNodAndShuffle.eOffset))
  checkAll("GmosNodAndShuffle.shuffle", LensTests(GmosNodAndShuffle.shuffle))
  checkAll("GmosNodAndShuffle.cycles",  LensTests(GmosNodAndShuffle.cycles))

  checkAll("GmosCommonStaticConfig.detector",      LensTests(GmosCommonStaticConfig.detector))
  checkAll("GmosCommonStaticConfig.mosPreImaging", LensTests(GmosCommonStaticConfig.mosPreImaging))
  checkAll("GmosCommonStaticConfig.nodAndShuffle", LensTests(GmosCommonStaticConfig.nodAndShuffle))
  checkAll("GmosCommonStaticConfig.customRois",    LensTests(GmosCommonStaticConfig.customRois))

  checkAll("GmosCcdReadout.xBinning",    LensTests(GmosCcdReadout.xBinning))
  checkAll("GmosCcdReadout.yBinning",    LensTests(GmosCcdReadout.yBinning))
  checkAll("GmosCcdReadout.ampCount",    LensTests(GmosCcdReadout.ampCount))
  checkAll("GmosCcdReadout.ampReadMode", LensTests(GmosCcdReadout.ampReadMode))

  checkAll("GmosCommonDynamicConfig.ccdReadout",   LensTests(GmosCommonDynamicConfig.ccdReadout))
  checkAll("GmosCommonDynamicConfig.dtaxOffset",   LensTests(GmosCommonDynamicConfig.dtaxOffset))
  checkAll("GmosCommonDynamicConfig.exposureTime", LensTests(GmosCommonDynamicConfig.exposureTime))
  checkAll("GmosCommonDynamicConfig.roi",          LensTests(GmosCommonDynamicConfig.roi))

  checkAll("GmosCustomMask.maskDefinitionFilename", LensTests(GmosCustomMask.maskDefinitionFilename))
  checkAll("GmosCustomMask.slitWidth",              LensTests(GmosCustomMask.slitWidth))

  checkAll("GmosGrating.disperser",  LensTests(GmosGrating.disperser[GmosNorthDisperser]))
  checkAll("GmosGrating.order",      LensTests(GmosGrating.order[GmosNorthDisperser]))
  checkAll("GmosGrating.wavelength", LensTests(GmosGrating.wavelength[GmosNorthDisperser]))

}
