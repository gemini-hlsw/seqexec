// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import edu.gemini.spModel.config2.{Config, DefaultConfig, ItemEntry, ItemKey}
import edu.gemini.spModel.seqcomp.SeqConfigNames
import org.scalacheck.{Arbitrary, _}
import org.scalacheck.Arbitrary._
import cats.tests.CatsSuite
import org.scalatest.EitherValues

trait ConfigArbitraries {

  implicit val arbItemKey: Arbitrary[ItemKey] =
    Arbitrary {
      for {
        prefix <- Gen.oneOf(Seq(SeqConfigNames.OBSERVE_KEY, SeqConfigNames.CALIBRATION_KEY, SeqConfigNames.TELESCOPE_KEY, SeqConfigNames.INSTRUMENT_KEY, SeqConfigNames.META_DATA_KEY, SeqConfigNames.OCS_KEY))
        suffix <- arbitrary[String]
      } yield new ItemKey(prefix, suffix)
    }

  // Will generate ItemEntry with only String values
  implicit val arbItemEntry: Arbitrary[ItemEntry] =
    Arbitrary {
      for {
        key   <- arbitrary[ItemKey]
        value <- arbitrary[String]
      } yield new ItemEntry(key, value)
    }

  implicit val arbConfig: Arbitrary[Config] =
    Arbitrary {
      for {
        items <- arbitrary[Array[ItemEntry]]
      } yield new DefaultConfig(items)
    }
}

class ConfigUtilSpec extends CatsSuite with EitherValues with ConfigArbitraries {
  import ConfigUtilOps._

  test("ConfigUtil: extract keys with the correct type") {
    forAll { (c: Config, k: ItemKey) =>
      // Make sure the key is present
      c.putItem(k, "value")
      c.extract(k).as[String] shouldBe Right("value")
    }
  }
  test("ConfigUtil: fail to extract keys with the wrong type") {
    forAll { (c: Config, k: ItemKey) =>
      c.putItem(k, "value")
      c.extract(k).as[Int].left.value should matchPattern {
        case ConversionError(_, _) =>
      }
    }
  }
  test("ConfigUtil: fail to extract unknown keys") {
    forAll { (c: Config, k: ItemKey) =>
      // Make sure the key is removed
      c.remove(k)
      c.extract(k).as[String].left.value should matchPattern {
        case KeyNotFound(_) =>
      }
    }
  }
}
