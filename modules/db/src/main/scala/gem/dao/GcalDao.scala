// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.implicits._
import gem.CoAdds
import gem.config.GcalConfig
import gem.config.GcalConfig.GcalLamp
import doobie._, doobie.implicits._
import gem.dao.meta._
import gem.enum.{GcalArc, GcalContinuum, GcalDiffuser, GcalFilter, GcalShutter}
import gem.enum.GcalArc.{ArArc, CuArArc, ThArArc, XeArc}
import java.time.Duration

/** DAO support for inserting `GcalConfig` in either step_gcal or
  * gcal (for smart gcal lookup).
  */
object GcalDao {
  import CoAddsMeta._
  import EnumeratedMeta._
  import TimeMeta._

  def insertStepGcal(stepId: Int, gcal: GcalConfig): ConnectionIO[Int] =
    Statements.insertStepGcal(stepId, gcal).run

  def bulkInsertSmartGcal(gs: Vector[GcalConfig]): fs2.Stream[ConnectionIO, Int] =
    Statements.bulkInsertSmartGcal.updateManyWithGeneratedKeys[Int]("gcal_id")(gs.map(Statements.GcalRow.fromGcalConfig))

  def selectStepGcal(stepId: Int): ConnectionIO[Option[GcalConfig]] =
    Statements.selectStepGcal(stepId).option.map(_.flatten)

  def selectSmartGcal(gcalId: Int): ConnectionIO[Option[GcalConfig]] =
    Statements.selectSmartGcal(gcalId).option.map(_.flatten)

  object Statements {

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
          GcalConfig(lamp, filter, diffuser, shutter, exposureTime, coadds)
        }
    }

    object GcalRow {
      def fromGcalConfig(c: GcalConfig): GcalRow = {
        val arcs: GcalArc => Boolean =
          c.arcs

        GcalRow(c.continuum, arcs(ArArc), arcs(CuArArc), arcs(ThArArc), arcs(XeArc), c.filter, c.diffuser, c.shutter, c.exposureTime, c.coadds)
      }
    }

    val bulkInsertSmartGcal: Update[GcalRow] = {
      val sql =
        """
          INSERT INTO gcal (continuum,
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

    def insertStepGcal(stepId: Int, gcal: GcalConfig): Update0 = {
      val arcs: GcalArc => Boolean = gcal.arcs
      sql"""INSERT INTO step_gcal (step_gcal_id, continuum, ar_arc, cuar_arc, thar_arc, xe_arc, filter, diffuser, shutter, exposure_time, coadds)
            VALUES ($stepId, ${gcal.continuum}, ${arcs(ArArc)}, ${arcs(CuArArc)}, ${arcs(ThArArc)}, ${arcs(XeArc)}, ${gcal.filter}, ${gcal.diffuser}, ${gcal.shutter}, ${gcal.exposureTime}, ${gcal.coadds})
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

    def selectStepGcal(id: Int): Query0[Option[GcalConfig]] =
      (selectFragment("step_gcal") ++
        fr"""WHERE step_gcal_id = $id""").query[GcalRow].map(_.toGcalConfig)

    def selectSmartGcal(id: Int): Query0[Option[GcalConfig]] =
      (selectFragment("gcal") ++
        fr"""WHERE gcal_id = $id""").query[GcalRow].map(_.toGcalConfig)
  }

}
