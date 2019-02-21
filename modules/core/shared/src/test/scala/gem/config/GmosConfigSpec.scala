// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.arb._
import gem.enum.GmosNorthDisperser
import gem.instances.time._

import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import monocle.law.discipline._

import GmosConfig._

final class GmosConfigSpec extends CatsSuite {

  import ArbEnumerated._
  import ArbGmos._
  import ArbOffset._
  import ArbTime._
  import ArbWavelength._

  // Laws

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
