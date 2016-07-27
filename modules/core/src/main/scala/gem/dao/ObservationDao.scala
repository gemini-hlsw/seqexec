package gem
package dao

import doobie.imports._

object ObservationDao {

  def insert(o: Observation): ConnectionIO[Int] =
    sql"""
      INSERT INTO observation (observation_id, 
                              program_id, 
                              observation_index, 
                              title,
                              instrument)
            VALUES (${o.id.toString}, 
                    ${o.id.pid.toString}, 
                    ${o.id.index}, 
                    ${o.title},
                    ${o.instrument})
    """.update.run

}
