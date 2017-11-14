// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import org.typelevel.discipline.scalatest.Discipline
import org.scalatest.FunSuite
import monocle.law.discipline.{LensTests, OptionalTests, PrismTests, TraversalTests}
import edu.gemini.seqexec.model.Model.{SeqexecEvent, SystemName}
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import SharedModelArbitraries._

import scalaz.std.AllInstances._

class ModelLensesSpec extends FunSuite with Discipline {

  // I'm not sure why these are not made available automatically
  implicit def arbF[A]: Arbitrary[A => A] = Arbitrary[A => A]((x: A) => x)

  checkAll("event observer name lens", LensTests(SeqexecEvent.obsNameL))
  checkAll("each view traversal", TraversalTests(SeqexecEvent.eachViewL))
  checkAll("sequence queue lens", LensTests(SeqexecEvent.sequencesQueueL))
  checkAll("queue view lens", LensTests(SeqexecEvent.ssLens))
  checkAll("events prism", PrismTests(SeqexecEvent.sePrism))
  checkAll("sequencename traversal", TraversalTests(SeqexecEvent.sequenceNameL))
  checkAll("steps lens", LensTests(SeqexecEvent.obsStepsL))
  checkAll("steps traversal", TraversalTests(SeqexecEvent.eachStepL))
  checkAll("standard step prism", PrismTests(SeqexecEvent.standardStepL))
  checkAll("standard step config", LensTests(SeqexecEvent.stepConfigL))
  checkAll("observe step config", LensTests(SeqexecEvent.systemConfigL(SystemName.observe)))
  checkAll("target name config", LensTests(SeqexecEvent.targetNameL("object")))
  checkAll("config target name config", OptionalTests(SeqexecEvent.configTargetNameL(SystemName.observe, "object")))
  checkAll("targetname traversal", TraversalTests(SeqexecEvent.sequenceTargetNameL))
}
