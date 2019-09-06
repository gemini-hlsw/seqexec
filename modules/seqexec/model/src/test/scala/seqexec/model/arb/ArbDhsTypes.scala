// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import seqexec.model.dhs._

trait ArbDhsTypes {

  implicit val arbImageFileId: Arbitrary[ImageFileId] =
    Arbitrary {
      Gen.alphaNumStr.map(toImageFileId)
    }

  implicit val imageFileIdCogen: Cogen[ImageFileId] =
    Cogen[String]
      .contramap(identity)

  implicit val arbDataId: Arbitrary[DataId] =
    Arbitrary {
      Gen.alphaNumStr.map(toDataId)
    }

  implicit val dataIdCogen: Cogen[DataId] =
    Cogen[String]
      .contramap(identity)
}

object ArbDhsTypes extends ArbDhsTypes
