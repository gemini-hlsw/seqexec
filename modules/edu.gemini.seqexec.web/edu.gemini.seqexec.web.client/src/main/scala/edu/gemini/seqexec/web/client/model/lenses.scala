// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client

import monocle.Traversal
import edu.gemini.seqexec.model.Model.SeqexecEvent._
import edu.gemini.seqexec.model.Model._

trait ModelLenses {

  // Composite lens to find the step config
  def firstScienceTargetNameL: Traversal[SequenceView, TargetName] =
    obsStepsL           ^|->> // observation steps
    eachStepL           ^<-?  // each step
    standardStepL       ^|->  // only standard steps
    stepConfigL         ^|->> // get step config
    scienceStepL        ^|-?  // science steps
    scienceTargetNameL        // science target name
}

object lenses extends ModelLenses
