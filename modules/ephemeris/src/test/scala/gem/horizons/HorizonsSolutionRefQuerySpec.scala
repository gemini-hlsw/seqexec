// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.{ EphemerisKey, HorizonsSolutionRef }

import gem.test.RespectIncludeTags
import gem.test.Tags.RequiresNetwork

import cats.tests.CatsSuite

final class HorizonsSolutionRefQuerySpec extends CatsSuite with RespectIncludeTags {

  import HorizonsSolutionRefQuerySpec._

  test("parse solution ref terminated by comma") {
    HorizonsSolutionRefQuery.parseSolutionRef(wild2, wild2Header) shouldEqual Some(HorizonsSolutionRef("JPL#K162/5"))
  }

  test("parse solution ref terminated by newline") {
    HorizonsSolutionRefQuery.parseSolutionRef(beer, beerHeader) shouldEqual Some(HorizonsSolutionRef("JPL#23"))
  }

  test("parse a major body without solution ref") {
    HorizonsSolutionRefQuery.parseSolutionRef(titan, titanHeader) shouldEqual Some(HorizonsSolutionRef("JUn 17, 2016"))
  }

  test("runs comet solution ref query", RequiresNetwork) {
    HorizonsSolutionRefQuery(wild2).lookup.unsafeRunSync.isDefined shouldBe true
  }

  test("runs asteroid solution ref query", RequiresNetwork) {
    HorizonsSolutionRefQuery(beer).lookup.unsafeRunSync.isDefined shouldBe true
  }

  test("runs major body solution ref query", RequiresNetwork) {
    HorizonsSolutionRefQuery(titan).lookup.unsafeRunSync.isDefined shouldBe true
  }
}

object HorizonsSolutionRefQuerySpec {

  val wild2: EphemerisKey.Horizons =
    EphemerisKey.Comet("81P")

  val wild2Header: String =
    """
      |*******************************************************************************
      |JPL/HORIZONS                     81P/Wild 2                2017-Sep-27 12:26:51
      |Rec #:900817 (+COV)   Soln.date: 2017-Jul-06_14:39:31   # obs: 4021 (2008-2017)
      |
      |IAU76/J2000 helio. ecliptic osc. elements (au, days, deg., period=Julian yrs):
      |
      |  EPOCH=  2455772.5 ! 2011-Jul-30.0000000 (TDB)    RMSW= n.a.
      |   EC= .5370816286507473   QR= 1.597991694337067   TP= 2455250.0788030997
      |   OM= 136.0976955732219   W= 41.75965127535293    IN= 3.237207856138221
      |   A= 3.451994548584135    MA= 80.282282601105     ADIST= 5.305997402831204
      |   PER= 6.4137697671158    N= .153673474           ANGMOM= .026959831
      |   DAN= 1.75367            DDN= 4.09807            L= 177.8118934
      |   B= 2.1553656            MOID= .60296798         TP= 2010-Feb-22.5788030997
      |
      |Comet physical (GM= km^3/s^2; RAD= km):
      |   GM= n.a.                RAD= 2.
      |   M1=  8.8      M2=  13.1     k1=  14.    k2=  5.      PHCOF=  .030
      |
      |Comet non-gravitational force model (AMRAT=m^2/kg;A1-A3=au/d^2;DT=days;R0=au):
      |   AMRAT=  0.                                      DT=  0.
      |   A1= 1.811725646257E-9   A2= 3.974820487201E-11  A3= 0.
      | Standard model:
      |   ALN=  .1112620426   NK=  4.6142   NM=  2.15     NN=  5.093    R0=  2.808
      |
      |COMET comments
      |1: soln ref.= JPL#K162/5, data arc: 2008-10-20 to 2017-06-24
      |2: k1=14., k2=5., phase coef.=0.03;
      |*******************************************************************************
    """.stripMargin

  val beer: EphemerisKey.Horizons =
    EphemerisKey.AsteroidNew("1971 UC1")

  val beerHeader: String =
    """
      |*******************************************************************************
      |JPL/HORIZONS                1896 Beer (1971 UC1)           2017-Sep-27 13:57:35
      |Rec #:  1896 (+COV)   Soln.date: 2017-May-15_14:17:06   # obs: 1566 (1949-2017)
      |
      |IAU76/J2000 helio. ecliptic osc. elements (au, days, deg., period=Julian yrs):
      |
      |  EPOCH=  2455763.5 ! 2011-Jul-21.00 (TDB)         Residual RMS= .33782
      |   EC= .2217996492354765   QR= 1.842496797109042   TP= 2455815.8047106094
      |   OM= 182.1795820773458   W=  180.1210417236072   IN= 2.220651628945542
      |   A= 2.367638096409141    MA= 345.8494963260648   ADIST= 2.892779395709239
      |   PER= 3.64318            N= .270539749           ANGMOM= .02580981
      |   DAN= 2.89278            DDN= 1.8425             L= 2.3005429
      |   B= -.0046903            MOID= .83910203         TP= 2011-Sep-11.3047106094
      |
      |Asteroid physical parameters (km, seconds, rotational period in hours):
      |   GM= n.a.                RAD= 2.6885             ROTPER= 3.3278
      |   H= 13.8                 G= .150                 B-V= n.a.
      |                           ALBEDO= .202            STYP= n.a.
      |
      |ASTEROID comments:
      |1: soln ref.= JPL#23
      |2: source=ORB
      |*******************************************************************************
    """.stripMargin

  val titan: EphemerisKey.Horizons =
    EphemerisKey.MajorBody(606)

  val titanHeader: String =
    """
      |*******************************************************************************
      | Revised: JUn 17, 2016              Titan / (Saturn)                        606
      |                         http://ssd.jpl.nasa.gov/?sat_phys_par
      |                           http://ssd.jpl.nasa.gov/?sat_elem
      |
      | SATELLITE PHYSICAL PROPERTIES:
      |  Mean Radius (km)       = 2575.5   +-  2.0  Density (g/cm^3) =  1.880 +- 0.004
      |  Mass (10^19 kg)        = 13455.3           Geometric Albedo =  0.2
      |  GM (km^3/s^2)          = 8978.13  +-  0.06  V(1,0)          = -1.2
      |
      | SATELLITE ORBITAL DATA:
      |  Semi-major axis, a (km)= 1221.87 (10^3)  Orbital period     = 15.945421 d
      |  Eccentricity, e        = 0.0288          Rotational period  =
      |  Inclination, i  (deg)  = 0.28
      |*******************************************************************************
    """.stripMargin
}
