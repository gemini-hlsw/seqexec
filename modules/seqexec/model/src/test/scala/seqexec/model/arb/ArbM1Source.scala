// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import seqexec.model.enum.M1Source

trait ArbM1Source {

  implicit val arbM1Source: Arbitrary[M1Source] =
    Arbitrary {
      Gen.oneOf[M1Source](M1Source.PWFS1,
                          M1Source.PWFS2,
                          M1Source.OIWFS,
                          M1Source.GAOS,
                          M1Source.HRWFS)
    }

  implicit val m1SourceCogen: Cogen[M1Source] =
    Cogen[String].contramap(_.productPrefix)

}

object ArbM1Source extends ArbM1Source
