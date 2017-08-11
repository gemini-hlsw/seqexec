// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import Model._
import Model.SeqexecEvent._
import Model.SeqexecModelUpdate
import org.scalacheck.Arbitrary._
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

/**
  * Tests the Monocle Lenses for Seqexec Events
  */
class SeqexecEventPrismSpec extends FlatSpec with Matchers with PropertyChecks {
  import SharedModelArbitraries._

  "Model lenses" should
    "be defined for all types with a queue" in {
      forAll { (e: SeqexecEvent) =>
        // This test should fail if we forget to update the prism when adding new events
        sePrism.set((e, SequencesQueue(Conditions.default, Some("Operator name"), Nil)))(e) should matchPattern {
          case e: SeqexecModelUpdate if e.view.queue.isEmpty =>
          case e: ConnectionOpenEvent                        =>
          case e: NewLogMessage                              =>
          case NullEvent                                     =>
        }
      }
    }
}
