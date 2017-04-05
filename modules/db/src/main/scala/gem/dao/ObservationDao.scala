package gem
package dao

import gem.enum.Instrument

import doobie.imports._

import scalaz._, Scalaz._

object ObservationDao {

  def insert(o: Observation[_]): ConnectionIO[Int] =
    Statements.insert(o).run

  /** Select all the observation ids associated with the given program. */
  def selectIds(pid: Program.Id): ConnectionIO[List[Observation.Id]] =
    Statements.selectIds(pid).list

  def selectFlat(id: Observation.Id): ConnectionIO[Observation[Nothing]] =
    Statements.selectFlat(id).unique

  def select(id: Observation.Id): ConnectionIO[Observation[Step[_]]] =
    for {
      on <- selectFlat(id)
      ss <- StepDao.selectAllEmpty(id)
    } yield on.copy(steps = ss.values)

  def selectAllFlat(pid: Program.Id): ConnectionIO[List[Observation[Nothing]]] =
    Statements.selectAllFlat(pid).list

  def selectAll(pid: Program.Id): ConnectionIO[List[Observation[Step[_]]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverseU(select)
    } yield oss

  object Statements {

    def insert(o: Observation[_]): Update0 =
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
      """.update

    def selectIds(pid: Program.Id): Query0[Observation.Id] =
      sql"""
        SELECT observation_id
          FROM observation
         WHERE program_id = $pid
      """.query[Observation.Id]

    def selectFlat(id: Observation.Id): Query0[Observation[Nothing]] =
      sql"""
        SELECT title, instrument
          FROM observation
         WHERE observation_id = ${id}
      """.query[(String, Option[Instrument])]
         .map { case (t, i) =>
           Observation(id, t, i, Nil)
         }

    def selectAllFlat(pid: Program.Id): Query0[Observation[Nothing]] =
      sql"""
        SELECT observation_index, title, instrument
          FROM observation
         WHERE program_id = ${pid}
      ORDER BY observation_index
      """.query[(Int, String, Option[Instrument])]
         .map { case (n, t, i) =>
           Observation(Observation.Id(pid, n), t, i, Nil)
         }

  }
}
