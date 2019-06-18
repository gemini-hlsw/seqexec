// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import seqexec.model.enum.TipTiltSource

trait ArbTipTiltSource {

  implicit val arbTipTiltSource: Arbitrary[TipTiltSource] =
    Arbitrary {
      Gen.oneOf[TipTiltSource](TipTiltSource.PWFS1,
                               TipTiltSource.PWFS2,
                               TipTiltSource.OIWFS,
                               TipTiltSource.GAOS)
    }

  implicit val tipTiltSourceCogen: Cogen[TipTiltSource] =
    Cogen[String].contramap(_.productPrefix)

}

object ArbTipTiltSource extends ArbTipTiltSource
