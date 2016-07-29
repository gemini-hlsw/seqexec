package gem
package dao

import doobie.imports._

import scalaz._, Scalaz._

object StepDao {

  def insert[I <: seq.Instrument /* : seq.Describe */](oid: Observation.Id, index: Int, s: seq.Step[I]): ConnectionIO[Int] =
    insertBaseSlice(oid, index, s) *> {
      s match {
        case x @ seq.BiasStep(_)       => insertBiasSlice(oid, index, x)
        case x @ seq.ScienceStep(_, _) => insertScienceSlice(oid, index, x)
        case x => FC.delay(println("Can't insert detail slice for " + x)).as(0)
      }
    }

  def insertBaseSlice[I <: seq.Instrument /* : seq.Describe */](oid: Observation.Id, index: Int, s: seq.Step[I]): ConnectionIO[Int] =
    sql"""
      INSERT INTO step (observation_id,
                        index,
                        instrument_id,
                        step_type)
      VALUES ($oid,
              ${index},
              ${s.instrument.tag},
              ${s.stepType.toString} :: step_type)
    """.update.run

  def insertBiasSlice[I <: seq.Instrument /* : seq.Describe */](oid: Observation.Id, index: Int, s: seq.BiasStep[I]): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_bias (observation_id,
                             index)
      VALUES (${oid.toString},
              ${index})
    """.update.run

  def insertScienceSlice[I <: seq.Instrument /* : seq.Describe */](oid: Observation.Id, index: Int, s: seq.ScienceStep[I]): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_science (observation_id,
                                index,
                                offset_p,
                                offset_q)
      VALUES (${oid},
              ${index},
              ${s.telescope.p},
              ${s.telescope.q})
    """.update.run

}