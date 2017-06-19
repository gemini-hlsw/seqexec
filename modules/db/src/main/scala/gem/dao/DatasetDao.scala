// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
