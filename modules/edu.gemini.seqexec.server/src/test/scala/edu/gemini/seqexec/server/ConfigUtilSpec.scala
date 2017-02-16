package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.{Config, DefaultConfig, ItemEntry, ItemKey}
import edu.gemini.spModel.seqcomp.SeqConfigNames
import org.scalacheck.{Arbitrary, _}
import org.scalacheck.Arbitrary._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{EitherValues, FlatSpec, Matchers}

import scalaz.{-\/, \/-}

object ConfigArbitraries {

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

class ConfigUtilSpec extends FlatSpec with Matchers with EitherValues with PropertyChecks {
  import ConfigUtilOps._
  import ConfigArbitraries._

  "ConfigUtil" should
    "extract keys with the correct type" in {
      forAll { (c: Config, k: ItemKey) =>
        // Make sure the key is present
        c.putItem(k, "value")
        c.extract(k).as[String] shouldBe \/-("value")
      }
    }
    it should "fail to extract keys with the wrong type" in {
      forAll { (c: Config, k: ItemKey) =>
        c.putItem(k, "value")
        c.extract(k).as[Int].toEither.left.value should matchPattern {
          case ConversionError(k,_) =>
        }
      }
    }
    it should "fail to extract unknown keys" in {
      forAll { (c: Config, k: ItemKey) =>
        // Make sure the key is removed
        c.remove(k)
        c.extract(k).as[String].toEither.left.value should matchPattern {
          case KeyNotFound(k) =>
        }
      }
    }
    it should "convert to StepConfig" in {
      forAll { (c: Config) =>
        // Not much to check but at least verify the amount of subsystems
        val subsystems = c.getKeys.map(_.getRoot.getName).distinct
        c.toStepConfig.keys should contain theSameElementsAs subsystems
      }
    }
}
