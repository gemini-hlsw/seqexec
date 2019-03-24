// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.syntax.functor._
import doobie._, doobie.implicits._
import gem.{ Dataset, Observation }
import gem.dao.meta._

object DatasetDao {
  import DatasetLabelMeta._
  import ObservationIdMeta._

  /**
   * Construct a program to insert the given a `Dataset`, associating it with the given step
   * (identified by its internal unique serial number). This program will raise a key violation
   * if the dataset label already exists in the database.
   */
  def insert(stepId: Int, d: Dataset): ConnectionIO[Unit] =
    Statements.insert(stepId, d).run.void

  /** Select all datasets (if any) for the specified observation, ordered by index. */
  def selectAll(oid: Observation.Id): ConnectionIO[List[Dataset]] =
    Statements.selectAll(oid).to[List]

  object Statements {

    // StepId has a DISTINCT type due to its check constraint so we need a fine-grained mapping
    // here to satisfy the query checker.
    private final case class StepId(toInt: Int)
    private object StepId {
      implicit val StepIdMeta: Meta[StepId] =
        Distinct.integer("id_index").timap(StepId(_))(_.toInt)
    }

    // This uses the internal unique serial number for steps, which is a code smell.
    def insert(stepId: Int, d: Dataset): Update0 =
      sql"""
        INSERT INTO dataset (dataset_label,
                             observation_id,
                             dataset_index,
                             step_id,
                             filename,
                             timestamp)
            VALUES (${d.label},
                    ${d.label.observationId},
                    ${StepId(d.label.index)},
                    ${stepId},
                    ${d.filename},
                    ${d.timestamp})
      """.update

    def selectAll(oid: Observation.Id): Query0[Dataset] =
      sql"""
        SELECT dataset_label, filename, timestamp
          FROM dataset
         WHERE observation_id = ${oid}
      ORDER BY dataset_index
      """.query[Dataset]

  }

}
