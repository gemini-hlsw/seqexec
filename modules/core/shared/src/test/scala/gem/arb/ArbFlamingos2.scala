// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.arb

import gem.config.F2Config.F2FpuChoice
import gem.config.{DynamicConfig, StaticConfig}
import gem.enum._
import gsp.math.arb.ArbTime
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen}

import java.time.Duration

trait ArbFlamingos2 {

  import ArbEnumerated._
  import ArbTime._

  // Static Config

  implicit val arbFlamingo2Static: Arbitrary[StaticConfig.Flamingos2] =
    Arbitrary(arbitrary[MosPreImaging].map(StaticConfig.Flamingos2(_)))

  // Dynamic Config

  implicit val arbF2FpuChoice: Arbitrary[F2FpuChoice] =
    Arbitrary {
      Gen.oneOf(Gen.const(F2FpuChoice.Custom),
                arbitrary[F2Fpu].map(F2FpuChoice.Builtin(_)))
    }

  implicit val cogF2FpuChoice: Cogen[F2FpuChoice] =
    Cogen[Option[F2Fpu]].contramap(_.toBuiltin)

  implicit val arbFlamingos2Dynamic: Arbitrary[DynamicConfig.Flamingos2] =
    Arbitrary {
      for {
        d <- arbitrary[Option[F2Disperser]]
        e <- arbitrary[Duration]
        f <- arbitrary[F2Filter]
        u <- arbitrary[Option[F2FpuChoice]]
        l <- arbitrary[F2LyotWheel]
        r <- arbitrary[F2ReadMode]
        w <- arbitrary[F2WindowCover]
      } yield DynamicConfig.Flamingos2(d, e, f, u, l, r, w)
    }

  implicit val cogFlamingos2Dynamic: Cogen[DynamicConfig.Flamingos2] =
    Cogen[(Option[F2Disperser], Duration, F2Filter, Option[F2FpuChoice], F2LyotWheel, F2ReadMode, F2WindowCover)]
      .contramap(f => (
        f.disperser,
        f.exposureTime,
        f.filter,
        f.fpu,
        f.lyotWheel,
        f.readMode,
        f.windowCover
      ))

}

object ArbFlamingos2 extends ArbFlamingos2
