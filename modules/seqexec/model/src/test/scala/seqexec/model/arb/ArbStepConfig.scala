// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.arb

import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen
import gem.arb.ArbEnumerated._
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

}

object ArbStepConfig extends ArbStepConfig
