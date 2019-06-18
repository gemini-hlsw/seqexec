// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import seqexec.model.enum.MountGuideOption

trait ArbMountGuideOption {

  implicit val arbMountGuideOption: Arbitrary[MountGuideOption] =
    Arbitrary {
      Gen.oneOf[MountGuideOption](MountGuideOption.MountGuideOn,
                                  MountGuideOption.MountGuideOff)
    }

  implicit val mountGuideCogen: Cogen[MountGuideOption] =
    Cogen[String].contramap(_.productPrefix)

}

object ArbMountGuideOption extends ArbMountGuideOption
