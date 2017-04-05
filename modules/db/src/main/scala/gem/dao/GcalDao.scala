package gem.dao

import gem.config.GcalConfig
import gem.config.GcalConfig.GcalLamp
import doobie.imports._
import gem.enum.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import gem.enum.GcalArc.{ArArc, CuArArc, ThArArc, XeArc}

import java.time.Duration

object GcalDao {

  def insert(gcal: GcalConfig, step: Option[Int]): ConnectionIO[Int] =
    Statements.insert(gcal, step).withUniqueGeneratedKeys[Int]("gcal_id") // janky, hm

  def select(id: Int): ConnectionIO[Option[GcalConfig]] =
    Statements.select(id).option.map(_.flatten)

  object Statements {

    def insert(gcal: GcalConfig, step: Option[Int]): Update0 = {
      val arcs: GcalArc => Boolean = gcal.arcs.member
      sql"""INSERT INTO gcal (step_id, continuum, ar_arc, cuar_arc, thar_arc, xe_arc, filter, diffuser, shutter, exposure_time, coadds)
            VALUES ($step, ${gcal.continuum}, ${arcs(ArArc)}, ${arcs(CuArArc)}, ${arcs(ThArArc)}, ${arcs(XeArc)}, ${gcal.filter}, ${gcal.diffuser}, ${gcal.shutter}, ${gcal.exposureTime}, ${gcal.coadds})
         """.update
    }

    def select(id: Int): Query0[Option[GcalConfig]] =
      sql"""
        SELECT continuum,
               ar_arc,
               cuar_arc,
               thar_arc,
               xe_arc,
               filter,
               diffuser,
               shutter,
               exposure_time,
               coadds
          FROM gcal
         WHERE gcal_id =$id
      """.query[GcalKernel].map(_.toGcalConfig)

      private case class GcalKernel(
        continuum: Option[GcalContinuum],
        ar_arc:    Boolean,
        cuar_arc:  Boolean,
        thar_arc:  Boolean,
        xe_arc:    Boolean,
        filter:    GcalFilter,
        diffuser:  GcalDiffuser,
        shutter:   GcalShutter,
        expTime:   Duration,
        coadds:    Short)
      {
        def toGcalConfig: Option[GcalConfig] =
          GcalLamp.fromConfig(continuum, ArArc -> ar_arc, CuArArc -> cuar_arc, ThArArc -> thar_arc, XeArc -> xe_arc).map { lamp =>
            GcalConfig(lamp, filter, diffuser, shutter, expTime, coadds.toInt)
          }
      }
  }

}
