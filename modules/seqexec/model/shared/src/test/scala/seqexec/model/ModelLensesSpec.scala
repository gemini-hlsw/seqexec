// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.tests.CatsSuite
import gem.arb.ArbEnumerated._
import gsp.math.laws.discipline.FormatTests
import gsp.math.arb.ArbOffset._
import gsp.math.arb.ArbAngle._
import gsp.math.Axis
import monocle.law.discipline.LensTests
import monocle.law.discipline.OptionalTests
import monocle.law.discipline.PrismTests
import monocle.law.discipline.TraversalTests
import org.scalacheck.Arbitrary._
import seqexec.model.enum._
import seqexec.model.SeqexecModelArbitraries._
import seqexec.model.SequenceEventsArbitraries._
import seqexec.model.arb.ArbStep._
import seqexec.model.arb.ArbStepState._
import seqexec.model.arb.ArbStandardStep._
import seqexec.model.arb.ArbNodAndShuffleStep._
import seqexec.model.arb.ArbStepConfig._

final class ModelLensesSpec extends CatsSuite with ModelLenses {
  checkAll("event observer name lens", LensTests(obsNameL))
  checkAll("each step traversal", TraversalTests(eachStepT))
  checkAll("observation steps lens", LensTests(obsStepsL))
  checkAll("each view traversal", TraversalTests(eachViewT))
  checkAll("sequence queue lens", LensTests(sessionQueueL))
  checkAll("events prism", PrismTests(sequenceEventsP))
  checkAll("param value lens", LensTests(paramValueL("object")))
  checkAll("system parameters lens",
           LensTests(systemConfigL(SystemName.Observe)))
  checkAll("config param value optional",
           OptionalTests(configParamValueO(SystemName.Observe, "object")))
  checkAll("sequence view Lens", LensTests(sequenceQueueViewL))
  checkAll("sequencename traversal", TraversalTests(sequenceNameT))
  checkAll("sequence config traversal", TraversalTests(sequenceConfigT))
  checkAll("science step traversal", TraversalTests(scienceStepT))
  checkAll("science target name optional", OptionalTests(scienceTargetNameO))
  checkAll("step type optional", OptionalTests(stepTypeO))
  checkAll("first science target name traversal",
           TraversalTests(firstScienceTargetNameT))
  checkAll("observe targetname traversal", TraversalTests(observeTargetNameT))
  checkAll("telescope targetname traversal",
           TraversalTests(telescopeTargetNameT))
  checkAll("first science step target name traversal",
           TraversalTests(firstScienceTargetNameT))
  checkAll("step type prism", PrismTests(stringToStepTypeP))
  checkAll("step step type optional", OptionalTests(stepTypeO))
  checkAll("telescope offset p optional",
           OptionalTests(offsetO[OffsetType.Telescope, Axis.P]))
  checkAll("telescope offset q optional",
           OptionalTests(offsetO[OffsetType.Telescope, Axis.Q]))
  checkAll("step double prism", PrismTests(stringToDoubleP))
  checkAll("param guiding prism", PrismTests(stringToGuidingP))
  checkAll("telescope guiding traversal", TraversalTests(telescopeGuidingWithT))
  checkAll("observe exposure time Optional",
           OptionalTests(observeExposureTimeO))
  checkAll("observe coadds Optional", OptionalTests(observeCoaddsO))
  checkAll("instrument fpu Optional", OptionalTests(instrumentFPUO))
  checkAll("instrument fpu mode Optional", OptionalTests(instrumentFPUModeO))
  checkAll("instrument fpu custom mask Optional",
           OptionalTests(instrumentFPUCustomMaskO))
  checkAll("instrument filter Optional", OptionalTests(instrumentFilterO))
  checkAll("instrument camera Optional", OptionalTests(instrumentCameraO))
  checkAll("instrument disperser Optional", OptionalTests(instrumentDisperserO))
  checkAll("instrument disperser lambda Optional",
           OptionalTests(instrumentDisperserLambdaO))
  checkAll("instrument observing mode Optional",
           OptionalTests(instrumentObservingModeO))
  checkAll("instrument disperser mode lambda Optional",
           OptionalTests(instrumentDisperserLambdaO))
  checkAll("instrument decker Optional",
           OptionalTests(instrumentDeckerO))
  checkAll("instrument imaging mirror Optional",
           OptionalTests(instrumentImagingMirrorO))
  checkAll("instrument mask Optional",
           OptionalTests(instrumentMaskO))
  checkAll("instrument read mode Optional",
           OptionalTests(instrumentReadModeO))
  checkAll("step class",
           OptionalTests(stepClassO))
  checkAll("StandardStep",
           PrismTests(Step.standardStepP))
  checkAll("NodAndShuffleStep",
           PrismTests(Step.nsStepP))
  checkAll("Step.status",
           LensTests(Step.status))
  checkAll("Step.config",
           LensTests(Step.config))
  checkAll("Step.id",
           LensTests(Step.id))
  checkAll("Step.skip",
           LensTests(Step.skip))
  checkAll("Step.breakpoint",
           LensTests(Step.breakpoint))
  checkAll("Step.observeStatus",
           OptionalTests(Step.observeStatus))
  checkAll("Step.configStatus",
           OptionalTests(Step.configStatus))
  checkAll("signedPFormat",
           FormatTests(signedComponentFormat[Axis.P]).formatWith(stringsOffsets))
  checkAll("signedQFormat",
           FormatTests(signedComponentFormat[Axis.Q]).formatWith(stringsOffsets))
  checkAll("signedArcsecFormat",
           FormatTests(signedArcsecFormat).formatWith(stringsOffsets))
}
