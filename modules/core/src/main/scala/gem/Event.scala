/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package gem

import java.time.Instant

sealed trait Event {
  def timestamp: Instant
  def oid: Observation.Id
}

object Event {

  case class StartSlew(       timestamp: Instant, oid: Observation.Id) extends Event
  case class EndSlew(         timestamp: Instant, oid: Observation.Id) extends Event

  case class StartVisit(      timestamp: Instant, oid: Observation.Id) extends Event
  case class EndVisit(        timestamp: Instant, oid: Observation.Id) extends Event

  case class StartSequence(   timestamp: Instant, oid: Observation.Id) extends Event
  case class EndSequence(     timestamp: Instant, oid: Observation.Id) extends Event

  case class PauseObserve(    timestamp: Instant, oid: Observation.Id) extends Event
  case class ContinueObserve( timestamp: Instant, oid: Observation.Id) extends Event
  case class AbortObserve(    timestamp: Instant, oid: Observation.Id) extends Event
  case class StopObserve(     timestamp: Instant, oid: Observation.Id) extends Event

  case class StartIntegration(timestamp: Instant, oid: Observation.Id, step: Int) extends Event
  case class EndIntegration(  timestamp: Instant, oid: Observation.Id, step: Int) extends Event


  def startSlew(t: Instant, o: Observation.Id): Event       = StartSlew(t, o)
  def endSlew(t: Instant, o: Observation.Id): Event         = EndSlew(t, o)

  def startVisit(t: Instant, o: Observation.Id): Event      = StartVisit(t, o)
  def endVisit(t: Instant, o: Observation.Id): Event        = EndVisit(t, o)

  def startSequence(t: Instant, o: Observation.Id): Event   = StartSequence(t, o)
  def endSequence(t: Instant, o: Observation.Id): Event     = EndSequence(t, o)

  def pauseObserve(t: Instant, o: Observation.Id): Event    = PauseObserve(t, o)
  def continueObserve(t: Instant, o: Observation.Id): Event = ContinueObserve(t, o)
  def abortObserve(t: Instant, o: Observation.Id): Event    = AbortObserve(t, o)
  def stopObserve(t: Instant, o: Observation.Id): Event     = StopObserve(t, o)

  def startIntegration(t: Instant, o: Observation.Id, i: Int): Event = StartIntegration(t, o, i)
  def endIntegration(t: Instant, o: Observation.Id, i: Int): Event   = EndIntegration(t, o, i)

}
