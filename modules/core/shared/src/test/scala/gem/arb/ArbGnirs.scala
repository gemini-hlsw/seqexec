// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.arb

import gem.CoAdds
import gem.config.{DynamicConfig, StaticConfig}
import gem.enum._
import gsp.math.Wavelength
import gsp.math.arb.{ ArbTime, ArbWavelength }

import java.time.Duration

import org.scalacheck.{ Arbitrary, Cogen }
import org.scalacheck.Arbitrary.arbitrary



trait ArbGnirs {

  import ArbCoAdds._
  import ArbEnumerated._
  import ArbTime._
  import ArbWavelength._

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
        k <- arbitrary[Wavelength]
      } yield DynamicConfig.Gnirs(a, b, c, d, e, f, g, h, i, j, k)
    }

  implicit val cogGnirsDynamic: Cogen[DynamicConfig.Gnirs] =
    Cogen[(GnirsAcquisitionMirror, GnirsCamera, CoAdds, GnirsDecker, GnirsDisperser, Duration, GnirsFilter, Either[GnirsFpuOther, GnirsFpuSlit], GnirsPrism, GnirsReadMode, Wavelength)]
      .contramap(g => (
        g.acquisitionMirror,
        g.camera,
        g.coadds,
        g.decker,
        g.disperser,
        g.exposureTime,
        g.filter,
        g.fpu,
        g.prism,
        g.readMode,
        g.wavelength
      ))

}

object ArbGnirs extends ArbGnirs
