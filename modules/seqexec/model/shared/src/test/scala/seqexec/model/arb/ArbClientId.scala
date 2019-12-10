// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import java.util.UUID
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import seqexec.model.ClientId

trait ArbClientId {

  implicit val clientIdArb: Arbitrary[ClientId] = Arbitrary {
    arbitrary[UUID].map(ClientId)
  }

  implicit val cogenUUID: Cogen[UUID] =
    Cogen[(Long, Long)].contramap(u =>
      (u.getMostSignificantBits, u.getLeastSignificantBits))

  implicit val cidCogen: Cogen[ClientId] =
    Cogen[UUID].contramap(_.self)

}

object ArbClientId extends ArbClientId
