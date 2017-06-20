// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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

    // CoAdds has a DISTINCT type due to its check constraint so we need a fine-grained mapping
    // here to satisfy the query checker.
    private final case class CoAdds(toShort: Short)
    private object CoAdds {
      implicit val StepIdMeta: Meta[CoAdds] =
        Distinct.short("coadds").xmap(CoAdds(_), _.toShort)
    }

    def insert(gcal: GcalConfig, step: Option[Int]): Update0 = {
      val arcs: GcalArc => Boolean = gcal.arcs.member
      sql"""INSERT INTO gcal (step_id, continuum, ar_arc, cuar_arc, thar_arc, xe_arc, filter, diffuser, shutter, exposure_time, coadds)
            VALUES ($step, ${gcal.continuum}, ${arcs(ArArc)}, ${arcs(CuArArc)}, ${arcs(ThArArc)}, ${arcs(XeArc)}, ${gcal.filter}, ${gcal.diffuser}, ${gcal.shutter}, ${gcal.exposureTime}, ${CoAdds(gcal.coadds)})
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

      private final case class GcalKernel(
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
            GcalConfig(lamp, filter, diffuser, shutter, expTime, coadds)
          }
      }
  }

}
