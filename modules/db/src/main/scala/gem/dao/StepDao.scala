package gem
package dao

import edu.gemini.spModel.core._
import gem.config._
import gem.enum._

import doobie.imports._
import scalaz._, Scalaz._

object StepDao {

  def insert[I <: InstrumentConfig](oid: Observation.Id, index: Int, s: Step[I]): ConnectionIO[Int] =
    insertBaseSlice(oid, index, s.instrument, s.stepType) *> {
      s match {
        case BiasStep(_)       => insertBiasSlice(oid, index)
        case DarkStep(_)       => insertDarkSlice(oid, index)
        case ScienceStep(_, t) => insertScienceSlice(oid, index, t)
        case GcalStep(_, g)    => insertGCalSlice(oid, index, g)
      }
    }

  def insertBaseSlice[I <: InstrumentConfig](oid: Observation.Id, index: Int, i: I, t: Step.Type): ConnectionIO[Int] =
    sql"""
      INSERT INTO step (observation_id, index, instrument, step_type)
      VALUES ($oid, ${index}, ${i.instrument.tag}, ${t.toString} :: step_type)
    """.update.run

  def insertBiasSlice(oid: Observation.Id, index: Int): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_bias (observation_id, index)
      VALUES (${oid.toString}, ${index})
    """.update.run

  def insertDarkSlice(oid: Observation.Id, index: Int): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_dark (observation_id, index)
      VALUES (${oid.toString}, ${index})
    """.update.run

  def insertGCalSlice(oid: Observation.Id, index: Int, gcal: GcalConfig): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_gcal (observation_id, index, gcal_lamp, shutter)
      VALUES (${oid.toString}, ${index}, ${gcal.lamp}, ${gcal.shutter} :: gcal_shutter)
    """.update.run

  def insertScienceSlice(oid: Observation.Id, index: Int, t: TelescopeConfig): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_science (observation_id, index, offset_p, offset_q)
      VALUES (${oid}, ${index}, ${t.p}, ${t.q})
    """.update.run



  // The type we get when we select the fully joined step
  private case class StepKernel(
    i: Instrument,
    stepType: String, // todo: make an enum
    gcal: (Option[GCalLamp], Option[GCalShutter]),
    telescope: (Option[OffsetP],  Option[OffsetQ])
  ) {
    def toStep: Step[_] =
      stepType match {
        
        case "Bias" => BiasStep(i)
        case "Dark" => DarkStep(i)

        case "Gcal" =>
          gcal match {
            case (Some(l), Some(s)) => GcalStep(i, GcalConfig(l, s))
            case _ => sys.error("missing gcal information: " + gcal)
          }

        case "Science" =>
          telescope match {
            case (Some(p), Some(q)) => ScienceStep(i, TelescopeConfig(p, q))
            case _ => sys.error("missing telescope information: " + telescope)
         }
      }
  }

  def selectAll(oid: Observation.Id): ConnectionIO[List[Step[_]]] =
    sql"""
      SELECT s.instrument,
             s.step_type,
             sg.gcal_lamp,
             sg.shutter,
             sc.offset_p,
             sc.offset_q
        FROM step s
             LEFT OUTER JOIN step_gcal sg
                ON sg.observation_id = s.observation_id AND sg.index = s.index
             LEFT OUTER JOIN step_science sc
                ON sc.observation_id = s.observation_id AND sc.index = s.index
       WHERE s.observation_id = ${oid}
    ORDER BY s.index
    """.query[StepKernel].map(_.toStep).list
    
}






// CREATE TYPE step_type AS ENUM
// (
//  'Bias',
//  'Dark',
//  'Gcal',
//  'Science',
//  'Smart'
// )



