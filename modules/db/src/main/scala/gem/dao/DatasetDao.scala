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
                    ${d.label.index},
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
