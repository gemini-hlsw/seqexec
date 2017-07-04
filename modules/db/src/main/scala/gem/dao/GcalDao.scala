// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import gem.config.GcalConfig
import gem.config.GcalConfig.GcalLamp
import doobie.imports._
import gem.enum.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import gem.enum.GcalArc.{ArArc, CuArArc, ThArArc, XeArc}

import scalaz._, Scalaz._

import java.time.Duration

object GcalDao {
  def insertGcal(stepId: Int, gcal: GcalConfig): ConnectionIO[Int] =
    Statements.insertGcal(stepId, gcal).withUniqueGeneratedKeys[Int]("gcal_id") // janky, hm

  def bulkInsertSmartGcal(gs: Vector[GcalConfig]): scalaz.stream.Process[ConnectionIO, Int] =
    Statements.bulkInsertSmartGcal.updateManyWithGeneratedKeys[Int]("smart_id")(gs.map(Statements.GcalRow.fromGcalConfig))

  def selectGcal(id: Int): ConnectionIO[Option[GcalConfig]] =
    Statements.selectGcal(id).option.map(_.flatten)

  def selectSmartGcal(id: Int): ConnectionIO[Option[GcalConfig]] =
    Statements.selectSmartGcal(id).option.map(_.flatten)

  object Statements {

    // CoAdds has a DISTINCT type due to its check constraint so we need a fine-grained mapping
    // here to satisfy the query checker.
    final case class CoAdds(toShort: Short)
    object CoAdds {
      implicit val MetaCoAdds: Meta[CoAdds] =
        Distinct.short("coadds").xmap(CoAdds(_), _.toShort)
    }

    final case class GcalRow(
      continuum:    Option[GcalContinuum],
      ar:           Boolean,
      cuar:         Boolean,
      thar:         Boolean,
      xe:           Boolean,
      filter:       GcalFilter,
      diffuser:     GcalDiffuser,
      shutter:      GcalShutter,
      exposureTime: Duration,
      coadds:       CoAdds)
    {
      def toGcalConfig: Option[GcalConfig] =
        GcalLamp.fromConfig(continuum, ArArc -> ar, CuArArc -> cuar, ThArArc -> thar, XeArc -> xe).map { lamp =>
          GcalConfig(lamp, filter, diffuser, shutter, exposureTime, coadds.toShort)
        }
    }

    object GcalRow {
      def fromGcalConfig(c: GcalConfig): GcalRow = {
        val arcs: GcalArc => Boolean =
          c.arcs.member

        GcalRow(c.continuum, arcs(ArArc), arcs(CuArArc), arcs(ThArArc), arcs(XeArc), c.filter, c.diffuser, c.shutter, c.exposureTime, CoAdds(c.coadds))
      }
    }

    val bulkInsertSmartGcal: Update[GcalRow] = {
      val sql =
        """
          INSERT INTO smartgcal (continuum,
                                 ar_arc,
                                 cuar_arc,
                                 thar_arc,
                                 xe_arc,
                                 filter,
                                 diffuser,
                                 shutter,
                                 exposure_time,
                                 coadds)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """
      Update[GcalRow](sql)
    }

    def insertGcal(stepId: Int, gcal: GcalConfig): Update0 = {
      val arcs: GcalArc => Boolean = gcal.arcs.member
      sql"""INSERT INTO gcal (step_id, continuum, ar_arc, cuar_arc, thar_arc, xe_arc, filter, diffuser, shutter, exposure_time, coadds)
            VALUES ($stepId, ${gcal.continuum}, ${arcs(ArArc)}, ${arcs(CuArArc)}, ${arcs(ThArArc)}, ${arcs(XeArc)}, ${gcal.filter}, ${gcal.diffuser}, ${gcal.shutter}, ${gcal.exposureTime}, ${CoAdds(gcal.coadds)})
         """.update
    }

    private def selectFragment(table: String): Fragment =
      Fragment.const(
        s"""
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
             FROM $table
         """
      )

    def selectGcal(id: Int): Query0[Option[GcalConfig]] =
      (selectFragment("gcal") ++
        fr"""WHERE gcal_id = $id""").query[GcalRow].map(_.toGcalConfig)

    def selectSmartGcal(id: Int): Query0[Option[GcalConfig]] =
      (selectFragment("smartgcal") ++
        fr"""WHERE smart_id = $id""").query[GcalRow].map(_.toGcalConfig)
  }

}
