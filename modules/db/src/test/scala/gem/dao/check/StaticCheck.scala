// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import gem.enum.Instrument.{ Flamingos2, GmosN }
import gem.config.StaticConfig
import gem.config.GmosConfig.GmosNodAndShuffle

class StaticCheck extends Check {
  import StaticConfigDao.Statements._

  "StaticDao.Statements" should
            "insertBaseSlice"           in check(insertBaseSlice(Flamingos2))

  it should "F2.insert"                 in check(F2.insert(0, StaticConfig.F2.Default))
  it should "F2.select"                 in check(F2.select(0))

  it should "Gmos.insertNorth"          in check(Gmos.insertNorth(0, StaticConfig.GmosNorth.Default))
  it should "Gmos.insertSouth"          in check(Gmos.insertSouth(0, StaticConfig.GmosSouth.Default))
  it should "Gmos.insertCustomRoiEntry" in check(Gmos.insertCustomRoiEntry(0, GmosN, Dummy.gmosCustomRoiEntry))
  it should "Gmos.insertNodAndShuffle"  in check(Gmos.insertNodAndShuffle(0, GmosN, GmosNodAndShuffle.Default))
  it should "Gmos.selectNorth"          in check(Gmos.selectNorth(0))
  it should "Gmos.selectSouth"          in check(Gmos.selectSouth(0))
  it should "Gmos.selectCustomRoiEntry" in check(Gmos.selectCustomRoiEntry(0, GmosN))
  it should "Gmos.selectNodAndShuffle"  in check(Gmos.selectNodAndShuffle(0, GmosN))
}
