// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import edu.gemini.spModel.config2.{DefaultConfig, ItemEntry, ItemKey}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import ConfigUtilOps._

class CleanConfigSpec extends AnyFlatSpec with Matchers {

  val k1: ItemKey = new ItemKey("instrument:param1")
  val k2: ItemKey = new ItemKey("instrument:param2")
  val k3: ItemKey = new ItemKey("instrument:param3")
  val strVal: String = "dummy"
  val jIntVal: java.lang.Integer = 1234

  class Daddy

  class Son extends Daddy

  val c0Val = new Daddy
  val c1Val = new Son

  val testConfig = new DefaultConfig(
    Array(
      new ItemEntry(k1, strVal),
      new ItemEntry(k2, c0Val)
    )
  )

   ignore must "fail construction if given a set of overrides with conflicting types" in {
    an [java.lang.AssertionError] shouldBe thrownBy {
      new CleanConfig(
        testConfig,
        Map(
          k1 -> (jIntVal:AnyRef)
        )
      )
    }
  }

  "CleanConfig" must "not thrown exception when constructing with overrides of the existing types" in {
    noException shouldBe thrownBy {
      new CleanConfig(
        testConfig,
        Map(
          k1 -> "mummy"
        )
      )
    }
  }

  it must "not thrown exception when constructing with overrides of derived types" in {
    noException shouldBe thrownBy {
      new CleanConfig(
        testConfig,
        Map(
          k2 -> c1Val
        )
      )
    }
  }

  it must "override values in config" in {
    val newVal = "ymmud"

    val clCfg = new CleanConfig(
      testConfig,
      Map(
        k1 -> newVal
      )
    )
    clCfg.extractAs[String](k1) shouldBe Right(newVal)

  }

}
