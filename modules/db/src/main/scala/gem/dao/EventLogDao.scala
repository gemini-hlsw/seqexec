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

package gem.dao

import gem.{Event, Observation}
import gem.Event._
import gem.enum.EventType
import gem.enum.EventType.{Abort, Continue, EndIntegration, EndSequence, EndSlew, EndVisit, Pause, StartIntegration, StartSequence, StartSlew, StartVisit, Stop}

import java.time.Instant
import doobie.imports._


object EventLogDao {

  def insertAbortObserve(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(Abort, oid, None).run

  def insertContinue(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(Continue, oid, None).run

  def insertEndIntegration(oid: Observation.Id, step: Int): ConnectionIO[Int] =
    Statements.insertEvent(EndIntegration, oid, Some(step)).run

  def insertEndSequence(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(EndSequence, oid, None).run

  def insertEndSlew(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(EndSlew, oid, None).run

  def insertEndVisit(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(EndVisit, oid, None).run

  def insertPauseObserve(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(Pause, oid, None).run

  def insertStartIntegration(oid: Observation.Id, step: Int): ConnectionIO[Int] =
    Statements.insertEvent(StartIntegration, oid, Some(step)).run

  def insertStartSequence(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(StartSequence, oid, None).run

  def insertStartSlew(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(StartSlew, oid, None).run

  def insertStartVisit(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(StartVisit, oid, None).run

  def insertStop(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(Stop, oid, None).run

  def selectAll(start: Instant, end: Instant): ConnectionIO[List[Event]] =
    Statements.selectAll(start, end).list

  object Statements {

    def insertEvent(t: EventType, oid: Observation.Id, step: Option[Int]): Update0 =
      sql"""
        INSERT INTO log_observe_event (event, observation_id, step)
             VALUES ($t :: evt_type,
                     $oid,
                     $step)
      """.update

    def selectAll(start: Instant, end: Instant): Query0[Event] =
      sql"""
        SELECT timestamp,
               event :: evt_type,
               observation_id,
               step
          FROM log_observe_event
         WHERE timestamp BETWEEN $start AND $end
      ORDER BY timestamp
      """.query[(Instant, EventType, Observation.Id, Option[Int])].map {
        case (t, Abort,            s, None   ) => abortObserve(t, s)
        case (t, Continue,         s, None   ) => continueObserve(t, s)
        case (t, EndIntegration,   s, Some(i)) => endIntegration(t, s, i)
        case (t, EndSequence,      s, None   ) => endSequence(t, s)
        case (t, EndSlew,          s, None   ) => endSlew(t, s)
        case (t, EndVisit,         s, None   ) => endVisit(t, s)
        case (t, Pause,            s, None   ) => pauseObserve(t, s)
        case (t, StartIntegration, s, Some(i)) => startIntegration(t, s, i)
        case (t, StartSequence,    s, None   ) => startSequence(t, s)
        case (t, StartSlew,        s, None   ) => startSlew(t, s)
        case (t, StartVisit,       s, None   ) => startVisit(t, s)
        case (t, Stop,             s, None   ) => stopObserve(t, s)
        case (t, e,                s, step   ) =>
          sys.error(s"Unexpected row in log_observe_event: ($t, $e, $s, $step)")
      }

  }

}
