package gem.dao

import gem.config.GcalConfig
import doobie.imports._
import gem.enum.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import gem.enum.GcalArc.{ArArc, CuArArc, ThArArc, XeArc}

import java.time.Duration

object GcalDao {

  def insert(gcal: GcalConfig): ConnectionIO[Int] = {
    val arcs: GcalArc => Boolean = gcal.arcs.member

    for {
      _ <- sql"""INSERT INTO gcal (continuum, ar_arc, cuar_arc, thar_arc, xe_arc, filter, diffuser, shutter, exposure_time, coadds)
                      VALUES (${gcal.continuum}, ${arcs(ArArc)}, ${arcs(CuArArc)}, ${arcs(ThArArc)}, ${arcs(XeArc)}, ${gcal.filter}, ${gcal.diffuser}, ${gcal.shutter}, ${gcal.exposureTime}, ${gcal.coadds})
              """.update.run
      id <- sql"select lastval()".query[Int].unique
    } yield id
  }

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
    coadds:    Int)
  {
    def toGcalConfig: Option[GcalConfig] =
      GcalConfig.mkLamp(continuum, ArArc -> ar_arc, CuArArc -> cuar_arc, ThArArc -> thar_arc, XeArc -> xe_arc).map { lamp =>
        GcalConfig(lamp, filter, diffuser, shutter, expTime, coadds)
      }
  }

  def select(id: Int): ConnectionIO[Option[GcalConfig]] =
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
    """.query[GcalKernel].option.map(_.flatMap(_.toGcalConfig))
}
