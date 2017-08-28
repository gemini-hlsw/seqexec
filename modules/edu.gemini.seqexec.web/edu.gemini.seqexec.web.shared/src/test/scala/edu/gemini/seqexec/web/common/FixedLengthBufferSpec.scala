// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.common

import org.scalatest.{FlatSpec, NonImplicitAssertions, Matchers}
import org.scalatest.prop.PropertyChecks

import scalaz.syntax.show._
import scalaz.syntax.equal._
import scalaz.syntax.functor._
import scalaz.std.AllInstances._

/**
  * Tests the Monocle Lenses for Seqexec Events
  */
class FixedLengthBufferSpec extends FlatSpec with Matchers with PropertyChecks with NonImplicitAssertions with ArbitrariesWebCommon {

  "FixedLenghBuffer" should
    "never overflow" in {
      forAll { (e: Int) =>
        val initial = FixedLengthBuffer.unsafeFromInt[Int](10)
        val buffer = { 0 to Math.abs(e % 10000) }.foldLeft(initial) {
          case (b, i) => b.append(i)
        }
        buffer.toVector.length should be<= 10
      }
    }
    it should "support a natural show" in {
      val initial = FixedLengthBuffer.unsafeFromInt[Int](10)
      val buffer = { 0 to 10 }.foldLeft(initial) {
        case (b, i) => b.append(i)
      }
      buffer.shows shouldBe buffer.toString
    }
    it should "support a natural Equal" in {
      val initial = FixedLengthBuffer.unsafeFromInt[Int](10)
      val buffer = { 0 until 10 }.foldLeft(initial) {
        case (b, i) => b.append(i)
      }
      val another = FixedLengthBuffer(10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      another.map(_ === buffer) should contain(true)
    }
    it should "support map identity" in {
      forAll { (e: FixedLengthBuffer[Int]) =>
        e.map(identity) === e shouldBe true
      }
    }
    it should "support map associativity" in {
      forAll { (e: FixedLengthBuffer[Int]) =>
        val f = (x: Int) => x * x
        val g = (x: Int) => x + x
        e.map(f map g) === e.map(f).map(g) shouldBe true
      }
    }
}
