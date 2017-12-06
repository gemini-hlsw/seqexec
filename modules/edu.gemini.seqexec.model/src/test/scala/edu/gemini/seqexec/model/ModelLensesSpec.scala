// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite
import monocle.law.discipline.{IsoTests, LensTests, OptionalTests, PrismTests, TraversalTests}
import edu.gemini.seqexec.model.Model.{OffsetAxis, SystemName}
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import SharedModelArbitraries._

import scalaz.std.AllInstances._

class ModelLensesSpec extends FunSuite with Discipline with ModelLenses {

  // I'm not sure why these are not made available automatically
  implicit def arbF[A]: Arbitrary[A => A] = Arbitrary[A => A]((x: A) => x)

  checkAll("event observer name lens", LensTests(obsNameL))
  checkAll("standard step prism", PrismTests(standardStepP))
  checkAll("each step traversal", TraversalTests(eachStepT))
  checkAll("observation steps lens", LensTests(obsStepsL))
  checkAll("each view traversal", TraversalTests(eachViewT))
  checkAll("sequence queue lens", LensTests(sequencesQueueL))
  checkAll("standard step config lens", LensTests(stepConfigL))
  checkAll("events prism", PrismTests(sequenceEventsP))
  checkAll("param value lens", LensTests(paramValueL("object")))
  checkAll("system parameters lens", LensTests(systemConfigL(SystemName.observe)))
  checkAll("config param value optional", OptionalTests(configParamValueO(SystemName.observe, "object")))
  checkAll("sequence view Lens", LensTests(sequenceQueueViewL))
  checkAll("sequencename traversal", TraversalTests(sequenceNameT))
  checkAll("sequence config traversal", TraversalTests(sequenceConfigT))
  checkAll("science step traversal", TraversalTests(scienceStepT))
  checkAll("science target name optional", OptionalTests(scienceTargetNameO))
  checkAll("step type optional", OptionalTests(stepTypeO))
  checkAll("first science target name traversal", TraversalTests(firstScienceTargetNameT))
  checkAll("observe targetname traversal", TraversalTests(observeTargetNameT))
  checkAll("telescope targetname traversal", TraversalTests(telescopeTargetNameT))
  checkAll("first science step target name traversal", TraversalTests(firstScienceTargetNameT))
  checkAll("step type prism", PrismTests(stringToStepTypeP))
  checkAll("step step type optional", OptionalTests(stepTypeO))
  checkAll("telescope p offset iso", IsoTests(telescopeOffsetPI))
  checkAll("telescope q offset iso", IsoTests(telescopeOffsetQI))
  checkAll("telescope offset optional", OptionalTests(telescopeOffsetO(OffsetAxis.AxisP)))
  checkAll("step double prism", PrismTests(stringToDoubleP))
  checkAll("param guiding prism", PrismTests(stringToGuidingP))
  checkAll("telescope guiding traversal", TraversalTests(telescopeGuidingWithT))
  checkAll("observe exposure time Optional", OptionalTests(observeExposureTimeO))
  checkAll("observe coadds Optional", OptionalTests(observeCoaddsO))
  checkAll("instrument fpu Optional", OptionalTests(instrumentFPUO))
  checkAll("instrument fpu mode Optional", OptionalTests(instrumentFPUModeO))
  checkAll("instrument fpu custom mask Optional", OptionalTests(instrumentFPUCustomMaskO))
}
