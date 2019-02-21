// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.arb

import gem.CoAdds
import gem.config.{DynamicConfig, StaticConfig}
import gem.enum._
import gem.math.Wavelength

import java.time.Duration

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Arbitrary.arbitrary



trait ArbGnirs {

  import ArbCoAdds._
  import ArbEnumerated._
  import ArbTime._

  // Static Config

  implicit val arbGnirsStatic: Arbitrary[StaticConfig.Gnirs] =
    Arbitrary(arbitrary[GnirsWellDepth].map(StaticConfig.Gnirs(_)))


  // Dynamic Config

  implicit val arbGnirsDynamic: Arbitrary[DynamicConfig.Gnirs] =
    Arbitrary {
      for {
        a <- arbitrary[GnirsAcquisitionMirror]
        b <- arbitrary[GnirsCamera]
        c <- arbitrary[CoAdds]
        d <- arbitrary[GnirsDecker]
        e <- arbitrary[GnirsDisperser]
        f <- arbitrary[Duration]
        g <- arbitrary[GnirsFilter]
        h <- arbitrary[Either[GnirsFpuOther, GnirsFpuSlit]]
        i <- arbitrary[GnirsPrism]
        j <- arbitrary[GnirsReadMode]
        k <- Gen.choose(1000, 120000).map(Wavelength.fromAngstroms.unsafeGet)
      } yield DynamicConfig.Gnirs(a, b, c, d, e, f, g, h, i, j, k)
    }

}

object ArbGnirs extends ArbGnirs
