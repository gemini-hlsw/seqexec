package gem
package dao

import gem.enum.Instrument

import doobie.imports._

object ObservationDao {

  def insert(o: Observation[_]): ConnectionIO[Int] =
    sql"""
      INSERT INTO observation (observation_id, 
                              program_id, 
                              observation_index, 
                              title,
                              instrument)
            VALUES (${o.id}, 
                    ${o.id.pid}, 
                    ${o.id.index}, 
                    ${o.title},
                    ${o.instrument})
    """.update.run

  def selectFlat(id: Observation.Id): ConnectionIO[Observation[Nothing]] =
    sql"""
      SELECT title, instrument
        FROM observation
       WHERE observation_id = ${id}
    """.query[(String, Option[Instrument])]
       .unique
       .map { case (t, i) =>
         Observation(id, t, i, Nil)
       }      

  def select(id: Observation.Id): ConnectionIO[Observation[Step[_]]] =
    for {
      on <- selectFlat(id)
      ss <- StepDao.selectAll(id)
    } yield on.copy(steps = ss)

  def selectAllFlat(pid: Program.Id): ConnectionIO[List[Observation[Nothing]]] =
    sql"""
      SELECT observation_index, title, instrument
        FROM observation
       WHERE program_id = ${pid}
    ORDER BY observation_index
    """.query[(Int, String, Option[Instrument])]
       .map { case (n, t, i) =>
         Observation(Observation.Id(pid, n), t, i, Nil)
       }
       .list

}
