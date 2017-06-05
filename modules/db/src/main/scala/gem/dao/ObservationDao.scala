package gem
package dao

import gem.config.StaticConfig
import gem.enum.Instrument

import doobie.imports._

import scalaz._, Scalaz._

object ObservationDao {

  def insert(o: Observation[StaticConfig, _]): ConnectionIO[Int] =
    Statements.insert(o).run

  /** Select all the observation ids associated with the given program. */
  def selectIds(pid: Program.Id): ConnectionIO[List[Observation.Id]] =
    Statements.selectIds(pid).list

  def selectFlat(id: Observation.Id): ConnectionIO[Observation[StaticConfig, Nothing]] =
    Statements.selectFlat(id).unique

  def select(id: Observation.Id): ConnectionIO[Observation[StaticConfig, Step[_]]] =
    for {
      on <- selectFlat(id)
      ss <- StepDao.selectAllEmpty(id)
    } yield on.copy(steps = ss.values)

  def selectAllFlat(pid: Program.Id): ConnectionIO[List[Observation[StaticConfig, Nothing]]] =
    Statements.selectAllFlat(pid).list

  def selectAll(pid: Program.Id): ConnectionIO[List[Observation[StaticConfig, Step[_]]]] =
    for {
      ids <- selectIds(pid)
      oss <- ids.traverseU(select)
    } yield oss

  object Statements {

    // ObservationIndex has a DISTINCT type due to its check constraint so we need a fine-grained mapping
    // here to satisfy the query checker.
    private case class ObservationIndex(toInt: Int)
    private object ObservationIndex {
      implicit val ObservationIndexMeta: Meta[ObservationIndex] =
        Distinct.integer("id_index").xmap(ObservationIndex(_), _.toInt)
    }

    def insert(o: Observation[StaticConfig, _]): Update0 =
      sql"""
        INSERT INTO observation (observation_id,
                                program_id,
                                observation_index,
                                title,
                                instrument)
              VALUES (${o.id},
                      ${o.id.pid},
                      ${ObservationIndex(o.id.index)},
                      ${o.title},
                      ${o.staticConfig.instrument : Instrument})
      """.update

    def selectIds(pid: Program.Id): Query0[Observation.Id] =
      sql"""
        SELECT observation_id
          FROM observation
         WHERE program_id = $pid
      """.query[Observation.Id]

    def selectFlat(id: Observation.Id): Query0[Observation[StaticConfig, Nothing]] =
      sql"""
        SELECT title, instrument
          FROM observation
         WHERE observation_id = ${id}
      """.query[(String, Instrument)]
         .map { case (t, i) =>
           Observation(id, t, StaticConfigDao.forInstrument(i), Nil)
         }

    def selectAllFlat(pid: Program.Id): Query0[Observation[StaticConfig, Nothing]] =
      sql"""
        SELECT observation_index, title, instrument
          FROM observation
         WHERE program_id = ${pid}
      ORDER BY observation_index
      """.query[(Short, String, Instrument)]
         .map { case (n, t, i) =>
           Observation(Observation.Id(pid, n.toInt), t, StaticConfigDao.forInstrument(i), Nil)
         }

  }
}
