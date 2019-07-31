// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

sealed trait GemsSource {
  val epicsVal: String
}

object GemsSource {
  case object Ttgs1 extends GemsSource { override val epicsVal = "TTGS1" }
  case object Ttgs2 extends GemsSource { override val epicsVal = "TTGS2" }
  case object Ttgs3 extends GemsSource { override val epicsVal = "TTGS3" }
  case object Odgw1 extends GemsSource { override val epicsVal = "ODGW1" }
  case object Odgw2 extends GemsSource { override val epicsVal = "ODGW2" }
  case object Odgw3 extends GemsSource { override val epicsVal = "ODGW3" }
  case object Odgw4 extends GemsSource { override val epicsVal = "ODGW4" }

  val all: List[GemsSource] = List(Ttgs1, Ttgs2, Ttgs3, Odgw1, Odgw2, Odgw3, Odgw4)
}