// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck._
import org.scalacheck.Cogen._
import gem.arb._
import gem.arb.ArbEnumerated._
import gsp.math.Angle
import gsp.math.Offset
import gsp.math.arb.ArbOffset._
import seqexec.model._
import seqexec.model.enum._

trait ArbStepConfig {
  val asciiStr: Gen[String] =
    Gen.listOf(Gen.alphaChar).map(_.mkString)

  val stepItemG: Gen[(String, String)] =
    for {
      a <- asciiStr
      b <- asciiStr
    } yield (a, b)

  val parametersGen: Gen[Parameters] =
    Gen.chooseNum(0, 10).flatMap(s => Gen.mapOfN[String, String](s, stepItemG))

  val stepConfigG: Gen[(SystemName, Parameters)] =
    for {
      a <- arbitrary[SystemName]
      b <- parametersGen
    } yield (a, b)

  val stepConfigGen: Gen[StepConfig] = Gen
    .chooseNum(0, 3)
    .flatMap(s => Gen.mapOfN[SystemName, Parameters](s, stepConfigG))

  implicit val stParams: Cogen[StepConfig] =
    Cogen[String].contramap(_.mkString(","))

   private val perturbations: List[String => Gen[String]] =
    List(
      s => if (s.startsWith("-")) Gen.const(s) else Gen.const(s"00%s") // insert leading 0s
    )

  // Strings that are often parsable as Offsets
  val stringsOffsets: Gen[String] =
    arbitrary[Offset]
      .map(x => Angle.arcseconds.get(x.p.toAngle).toString)
      .flatMapOneOf(Gen.const, perturbations: _*)
}

object ArbStepConfig extends ArbStepConfig
