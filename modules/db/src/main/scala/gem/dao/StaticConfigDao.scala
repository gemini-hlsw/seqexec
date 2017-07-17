// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie.imports._

import gem.enum.{GmosDetector, Instrument, MosPreImaging}
import gem.config._

import Gmos.{GmosCommonStaticConfig => GmosCommonSC}

import scalaz._
import Scalaz._


object StaticConfigDao {

  def insert(s: StaticConfig): ConnectionIO[Int] =
    for {
      id <- Statements.insertBaseSlice(s.instrument).withUniqueGeneratedKeys[Int]("static_id")
      _  <- insertConfigSlice(id, s)
    } yield id

  private def insertConfigSlice(id: Int, s: StaticConfig): ConnectionIO[Int] =
    s match {
      case _:  StaticConfig.AcqCam    => 0.point[ConnectionIO]
      case _:  StaticConfig.Bhros     => 0.point[ConnectionIO]
      case f2: StaticConfig.F2       => Statements.insertF2(id, f2).run
      case g:  StaticConfig.GmosNorth => Statements.insertGmosNorth(id, g).run
      case g:  StaticConfig.GmosSouth => Statements.insertGmosSouth(id, g).run
      case _:  StaticConfig.Gnirs     => 0.point[ConnectionIO]
      case _:  StaticConfig.Gpi       => 0.point[ConnectionIO]
      case _:  StaticConfig.Gsaoi     => 0.point[ConnectionIO]
      case _:  StaticConfig.Michelle  => 0.point[ConnectionIO]
      case _:  StaticConfig.Nici      => 0.point[ConnectionIO]
      case _:  StaticConfig.Nifs      => 0.point[ConnectionIO]
      case _:  StaticConfig.Niri      => 0.point[ConnectionIO]
      case _:  StaticConfig.Phoenix   => 0.point[ConnectionIO]
      case _:  StaticConfig.Trecs     => 0.point[ConnectionIO]
      case _:  StaticConfig.Visitor   => 0.point[ConnectionIO]
    }

  def select(i: Instrument, sid: Int): ConnectionIO[StaticConfig] = {
    def point(sc: StaticConfig): ConnectionIO[StaticConfig] =
      sc.point[ConnectionIO]

    i match {
      case Instrument.AcqCam     => point(StaticConfig.AcqCam())
      case Instrument.Bhros      => point(StaticConfig.Bhros())

      case Instrument.Flamingos2 => Statements.selectF2(sid)       .unique.widen[StaticConfig]
      case Instrument.GmosN      => Statements.selectGmosNorth(sid).unique.widen[StaticConfig]
      case Instrument.GmosS      => Statements.selectGmosSouth(sid).unique.widen[StaticConfig]

      case Instrument.Gnirs      => point(StaticConfig.Gnirs())
      case Instrument.Gpi        => point(StaticConfig.Gpi())
      case Instrument.Gsaoi      => point(StaticConfig.Gsaoi())
      case Instrument.Michelle   => point(StaticConfig.Michelle())
      case Instrument.Nici       => point(StaticConfig.Nici())
      case Instrument.Nifs       => point(StaticConfig.Nifs())
      case Instrument.Niri       => point(StaticConfig.Niri())
      case Instrument.Phoenix    => point(StaticConfig.Phoenix())
      case Instrument.Trecs      => point(StaticConfig.Trecs())
      case Instrument.Visitor    => point(StaticConfig.Visitor())
    }
  }


  object Statements {
    def selectF2(sid: Int): Query0[StaticConfig.F2] =
      sql"""
        SELECT mos_preimaging
          FROM static_f2
         WHERE static_id = $sid AND instrument = ${Instrument.Flamingos2: Instrument}
      """.query[StaticConfig.F2]

    // We need to define this explicitly because we're ignoring the nod and
    // shuffle bit for now.
    implicit val GmosCommonStaticComposite: Composite[GmosCommonSC] =
      Composite[(GmosDetector, MosPreImaging)].xmap(
        (t: (GmosDetector, MosPreImaging)) => GmosCommonSC(t._1, t._2, None),
        (s: GmosCommonSC)                  => (s.detector, s.mosPreImaging)
      )

    def selectGmosNorth(sid: Int): Query0[StaticConfig.GmosNorth] =
      sql"""
        SELECT detector,
               mos_preimaging,
               stage_mode
          FROM static_gmos_north
         WHERE static_id = $sid AND instrument = ${Instrument.GmosN: Instrument}
      """.query[StaticConfig.GmosNorth]

    def selectGmosSouth(sid: Int): Query0[StaticConfig.GmosSouth] =
      sql"""
        SELECT detector,
               mos_preimaging,
               stage_mode
          FROM static_gmos_south
         WHERE static_id = $sid AND instrument = ${Instrument.GmosS: Instrument}
      """.query[StaticConfig.GmosSouth]

    def insertBaseSlice(i: Instrument): Update0 =
      sql"""
        INSERT INTO static_config (instrument)
        VALUES ($i)
      """.update

    def insertF2(id: Int, f2: StaticConfig.F2): Update0 =
      sql"""
        INSERT INTO static_f2 (static_id, instrument, mos_preimaging)
        VALUES (
          $id,
          ${Instrument.Flamingos2: Instrument},
          ${f2.mosPreImaging})
      """.update

    def insertGmosNorth(id: Int, g: StaticConfig.GmosNorth): Update0 =
      sql"""
        INSERT INTO static_gmos_north (static_id, instrument, detector, mos_preimaging, stage_mode)
        VALUES (
          $id,
          ${Instrument.GmosN: Instrument},
          ${g.common.detector},
          ${g.common.mosPreImaging},
          ${g.stageMode})
      """.update

    def insertGmosSouth(id: Int, g: StaticConfig.GmosSouth): Update0 =
      sql"""
        INSERT INTO static_gmos_south (static_id, instrument, detector, mos_preimaging, stage_mode)
        VALUES (
          $id,
          ${Instrument.GmosS: Instrument},
          ${g.common.detector},
          ${g.common.mosPreImaging},
          ${g.stageMode})
      """.update
  }

}
