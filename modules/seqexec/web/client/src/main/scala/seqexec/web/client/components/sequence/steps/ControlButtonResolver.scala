// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import seqexec.model.{SequenceState, Step}
import seqexec.web.client.model.ClientStatus

sealed trait ControlButtonResolver[A] {
  def extractor(a: A): (ClientStatus, SequenceState, Step)

  def controlButtonsActive(a: A): Boolean = {
    val (clientStatus, state, step) = extractor(a)
    clientStatus.isLogged && state.isRunning &&
      (step.isObserving || step.isObservePaused || state.userStopRequested)
  }
}

object ControlButtonResolver {
  def build[A](extractorFn: A => (ClientStatus, SequenceState, Step)): ControlButtonResolver[A] =
    new ControlButtonResolver[A] {
      override def extractor(a: A): (ClientStatus, SequenceState, Step) = extractorFn(a)
    }
}

