package gem
package dao

import doobie.imports._
import scalaz._, Scalaz._

object StepDao {
  import gem.seq._

  def insert[I <: Instrument](oid: Observation.Id, index: Int, s: Step[I]): ConnectionIO[Int] =
    insertBaseSlice(oid, index, s.instrument, s.stepType) *> {
      s match {
        case BiasStep(_)       => insertBiasSlice(oid, index)
        case DarkStep(_)       => insertDarkSlice(oid, index)
        case ScienceStep(_, t) => insertScienceSlice(oid, index, t)
        case GcalStep(_, g)    => insertGCalSlice(oid, index, g)
      }
    }

  def insertBaseSlice[I <: Instrument](oid: Observation.Id, index: Int, i: Instrument, t: Step.Type): ConnectionIO[Int] =
    sql"""
      INSERT INTO step (observation_id, index, instrument_id, step_type)
      VALUES ($oid, ${index}, ${i.tag}, ${t.toString} :: step_type)
    """.update.run

  def insertBiasSlice[I <: Instrument](oid: Observation.Id, index: Int): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_bias (observation_id, index)
      VALUES (${oid.toString}, ${index})
    """.update.run

  def insertDarkSlice[I <: Instrument](oid: Observation.Id, index: Int): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_dark (observation_id, index)
      VALUES (${oid.toString}, ${index})
    """.update.run

  def insertGCalSlice[I <: Instrument](oid: Observation.Id, index: Int, gcal: GcalUnit): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_gcal (observation_id, index, gcal_lamp_id, shutter)
      VALUES (${oid.toString}, ${index}, ${gcal.lamp}, ${gcal.shutter} :: gcal_shutter)
    """.update.run

  def insertScienceSlice[I <: Instrument](oid: Observation.Id, index: Int, t: Telescope): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_science (observation_id, index, offset_p, offset_q)
      VALUES (${oid}, ${index}, ${t.p}, ${t.q})
    """.update.run

}
