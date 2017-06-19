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

import gem.{Dataset, Observation}

import doobie.imports._

import java.time.Instant

object DatasetDao {

  def insert(sid: Int, d: Dataset): ConnectionIO[Int] =
    Statements.insert(sid, d).run

  def selectAll(oid: Observation.Id): ConnectionIO[List[Dataset]] =
    Statements.selectAll(oid).list

  object Statements {

    // StepId has a DISTINCT type due to its check constraint so we need a fine-grained mapping
    // here to satisfy the query checker.
    private case class StepId(toInt: Int)
    private object StepId {
      implicit val StepIdMeta: Meta[StepId] =
        Distinct.integer("id_index").xmap(StepId(_), _.toInt)
    }

    def insert(sid: Int, d: Dataset): Update0 =
      sql"""
        INSERT INTO dataset (dataset_label,
                             observation_id,
                             dataset_index,
                             step_id,
                             filename,
                             timestamp)
            VALUES (${d.label},
                    ${d.label.oid},
                    ${StepId(d.label.index)},
                    ${sid},
                    ${d.filename},
                    ${d.timestamp})
      """.update

    def selectAll(oid: Observation.Id): Query0[Dataset] =
      sql"""
        SELEcT dataset_label, filename, timestamp
          FROM dataset
         WHERE observation_id = ${oid}
      ORDER BY dataset_index
      """.query[(Dataset.Label, String, Instant)]
         .map { case (l, f, t) => Dataset(l, f, t) }

  }

}
