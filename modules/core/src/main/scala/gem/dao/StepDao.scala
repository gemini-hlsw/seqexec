package gem
package dao

import doobie.imports._

object StepDao {

  def insert(s: Step): ConnectionIO[Int] =
    sql"""
      INSERT INTO step (observation_id,
                        observation_index,
                        program_id,
                        metadata_stepcount,
                        metadata_complete,
                        obs_class_id,
                        "observe_dataLabel",
                        observe_object,
                        "observe_observeType",
                        "observe_sciBand",
                        instrument_instrument)
      VALUES (${s.id.oid.toString},
              ${s.id.oid.index},
              ${s.id.oid.pid.toString},
              ${s.id.index},
              ${s.isComplete},
              ${s.observeClass.map(_.tag)},
              ${s.dataLabel},
              ${s.target},
              ${s.observeType},
              ${s.band},
              ${s.instrument.map(_.tag)})
    """.update.run

}