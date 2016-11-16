package gem
package dao

import edu.gemini.spModel.core._
import gem.config._
import gem.enum._

import doobie.imports._
import scalaz._, Scalaz._

object StepDao {

  def insert[I <: InstrumentConfig](oid: Observation.Id, index: Int, s: Step[I]): ConnectionIO[Int] =
    insertBaseSlice(oid, index, s.instrument, StepType.forStep(s)) *> {
      s match {
        case BiasStep(_)       => insertBiasSlice(oid, index)
        case DarkStep(_)       => insertDarkSlice(oid, index)
        case ScienceStep(_, t) => insertScienceSlice(oid, index, t)
        case GcalStep(_, g)    => insertGcalSlice(oid, index, g)
      }
    } *> insertConfigSlice(oid, index, s.instrument)

  private def insertBaseSlice(oid: Observation.Id, index: Int, i: InstrumentConfig, t: StepType): ConnectionIO[Int] =
    sql"""
      INSERT INTO step (observation_id, index, instrument, step_type)
      VALUES ($oid, ${index}, ${Instrument.forConfig(i).tag}, ${t.tag} :: step_type)
    """.update.run

  private def insertBiasSlice(oid: Observation.Id, index: Int): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_bias (observation_id, index)
      VALUES (${oid.toString}, ${index})
    """.update.run

  private def insertDarkSlice(oid: Observation.Id, index: Int): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_dark (observation_id, index)
      VALUES (${oid.toString}, ${index})
    """.update.run

  private def insertGcalSlice(oid: Observation.Id, index: Int, gcal: GcalConfig): ConnectionIO[Int] = {
    val continuum: Option[GcalContinuum] = gcal.lamp.swap.toOption
    val arcs: Set[GcalArc]               = gcal.lamp.getOrElse(Set.empty[GcalArc])

    import GcalArc._

    sql"""
      INSERT INTO step_gcal (observation_id, index, gcal_continuum, gcal_ar_arc, gcal_cuar_arc, gcal_thar_arc, gcal_xe_arc, shutter)
      VALUES (${oid.toString}, $index, $continuum, ${arcs(ArArc)}, ${arcs(CuArArc)}, ${arcs(ThArArc)}, ${arcs(XeArc)}, ${gcal.shutter} :: gcal_shutter)
    """.update.run
  }

  private def insertScienceSlice(oid: Observation.Id, index: Int, t: TelescopeConfig): ConnectionIO[Int] =
    sql"""
      INSERT INTO step_science (observation_id, index, offset_p, offset_q)
      VALUES (${oid}, ${index}, ${t.p}, ${t.q})
    """.update.run

    private def insertConfigSlice(oid: Observation.Id, index: Int, i: InstrumentConfig): ConnectionIO[Int] =
      i match {

        case F2Config(fpu, mosPreimaging, exposureTime, filter, lyotWheel, disperser, windowCover) =>
          sql"""
            INSERT INTO step_f2 (observation_id, index, fpu, mos_preimaging, exposure_time, filter, lyot_wheel, disperser, window_cover)
            VALUES ($oid, $index, $fpu, $mosPreimaging, ${exposureTime.getSeconds}, $filter, $lyotWheel, $disperser, $windowCover)
          """.update.run

        case GenericConfig(i) => 0.point[ConnectionIO]

      }

  // The type we get when we select the fully joined step
  private case class StepKernel(
    i: Instrument,
    stepType: StepType, // todo: make an enum
    gcal: (Option[GcalContinuum], Boolean, Boolean, Boolean, Boolean, Option[GcalShutter]),
    telescope: (Option[OffsetP],  Option[OffsetQ])
  ) {
    def toStep: Step[Instrument] =
      stepType match {

        case StepType.Bias => BiasStep(i)
        case StepType.Dark => DarkStep(i)

        case StepType.Gcal =>
          import GcalArc._
          val (continuumOpt, ar, cuar, thar, xe, shutterOpt) = gcal
          val lamp = continuumOpt.toLeftDisjunction {
            val arcs = Set[(GcalArc, Boolean)](ArArc -> ar, CuArArc -> cuar, ThArArc -> thar, XeArc -> xe)
            arcs.filter(_._2).unzip._1
          }
          shutterOpt.map(GcalConfig(lamp, _))
                    .map(GcalStep(i, _))
                    .getOrElse(sys.error("missing gcal information: " + gcal))

        case StepType.Science =>
          telescope.apply2(TelescopeConfig(_, _))
            .map(ScienceStep(i, _))
            .getOrElse(sys.error("missing telescope information: " + telescope))

      }
  }

  def selectAll(oid: Observation.Id): ConnectionIO[List[Step[Instrument]]] =
    sql"""
      SELECT s.instrument,
             s.step_type,
             sg.gcal_continuum,
             sg.gcal_ar_arc,
             sg.gcal_cuar_arc,
             sg.gcal_thar_arc,
             sg.gcal_xe_arc,
             sg.shutter,
             sc.offset_p,
             sc.offset_q
        FROM step s
             LEFT OUTER JOIN step_gcal sg
                ON sg.observation_id = s.observation_id AND sg.index = s.index
             LEFT OUTER JOIN step_science sc
                ON sc.observation_id = s.observation_id AND sc.index = s.index
       WHERE s.observation_id = $oid
    ORDER BY s.index
    """.query[StepKernel].map(_.toStep).list

}
