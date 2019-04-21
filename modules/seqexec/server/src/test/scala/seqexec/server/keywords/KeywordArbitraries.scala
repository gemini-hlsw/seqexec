// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import gem.enum.KeywordName
import gem.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait KeywordArbitraries extends ArbEnumerated {
  implicit val keywordTypeArb: Arbitrary[KeywordType] = Arbitrary {
    Gen.oneOf(TypeInt8,
              TypeInt16,
              TypeInt32,
              TypeFloat,
              TypeDouble,
              TypeBoolean,
              TypeString)
  }
  implicit val keywordTypeCogen: Cogen[KeywordType] =
    Cogen[String].contramap(_.productPrefix)

  implicit val internalKeywordArb: Arbitrary[InternalKeyword] = Arbitrary {
    for {
      name  <- arbitrary[KeywordName]
      kt    <- arbitrary[KeywordType]
      value <- Gen.listOfN(17, Gen.alphaChar)
    } yield InternalKeyword(name, kt, value.mkString)
  }
  implicit val internalKeywordCogen: Cogen[InternalKeyword] =
    Cogen[(KeywordName, KeywordType, String)].contramap(x =>
      (x.name, x.keywordType, x.value))

  implicit val keywordBagArb: Arbitrary[KeywordBag] = Arbitrary {
    arbitrary[List[InternalKeyword]].map(KeywordBag.apply)
  }
  implicit val keywordBagCogen: Cogen[KeywordBag] =
    Cogen[List[InternalKeyword]].contramap(_.keywords)
}
