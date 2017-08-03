// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie.imports._
import gem.enum.{ GmosDetector, Instrument, MosPreImaging }
import gem.config._

import scalaz._
import Scalaz._


object StaticConfigDao {

  def insert(s: StaticConfig): ConnectionIO[Int] =
    for {
      id <- Statements.insertBaseSlice(s.instrument).withUniqueGeneratedKeys[Int]("static_id")
      _  <- insertConfigSlice(id, s)
    } yield id

  private def insertConfigSlice(id: Int, s: StaticConfig): ConnectionIO[Unit] =
    s match {
      case _:  StaticConfig.AcqCam    => ().point[ConnectionIO]
      case _:  StaticConfig.Bhros     => ().point[ConnectionIO]
      case f2: StaticConfig.F2        => Statements.F2.insert(id, f2).run.void
      case g:  StaticConfig.GmosNorth => Gmos.insertNorth(id, g)
      case g:  StaticConfig.GmosSouth => Gmos.insertSouth(id, g)
      case _:  StaticConfig.Gnirs     => ().point[ConnectionIO]
      case _:  StaticConfig.Gpi       => ().point[ConnectionIO]
      case _:  StaticConfig.Gsaoi     => ().point[ConnectionIO]
      case _:  StaticConfig.Michelle  => ().point[ConnectionIO]
      case _:  StaticConfig.Nici      => ().point[ConnectionIO]
      case _:  StaticConfig.Nifs      => ().point[ConnectionIO]
      case _:  StaticConfig.Niri      => ().point[ConnectionIO]
      case _:  StaticConfig.Phoenix   => ().point[ConnectionIO]
      case _:  StaticConfig.Trecs     => ().point[ConnectionIO]
      case _:  StaticConfig.Visitor   => ().point[ConnectionIO]
    }

  def select(i: Instrument, sid: Int): ConnectionIO[StaticConfig] = {
    def point(sc: StaticConfig): ConnectionIO[StaticConfig] =
      sc.point[ConnectionIO]

    i match {
      case Instrument.AcqCam     => point(StaticConfig.AcqCam())
      case Instrument.Bhros      => point(StaticConfig.Bhros())

      case Instrument.Flamingos2 => Statements.F2.select(sid).unique.widen[StaticConfig]
      case Instrument.GmosN      => Gmos.selectNorth(sid)           .widen[StaticConfig]
      case Instrument.GmosS      => Gmos.selectSouth(sid)           .widen[StaticConfig]

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

  /** Combines lower-level GMOS statements into higher-level inserts and
    * selects.
    */
  private object Gmos {

    import gem.config.Gmos.GmosNodAndShuffle
    import gem.enum.Instrument.{ GmosN, GmosS }
    import StaticConfig.{ GmosNorth, GmosSouth }

    def insertNorth(sid: Int, gn: GmosNorth): ConnectionIO[Unit] =
        insertNodAndShuffle(sid, GmosN, gn.common.nodAndShuffle) *>
          Statements.Gmos.insertNorth(sid, gn).run.void

    def insertSouth(sid: Int, gs: GmosSouth): ConnectionIO[Unit] =
        insertNodAndShuffle(sid, GmosS, gs.common.nodAndShuffle) *>
          Statements.Gmos.insertSouth(sid, gs).run.void

    def insertNodAndShuffle(sid: Int, i: Instrument, ns: Option[GmosNodAndShuffle]): ConnectionIO[Unit] =
      ns.fold(().point[ConnectionIO])(ns => Statements.Gmos.insertNodAndShuffle(sid, i, ns).run.void)

    def selectNorth(sid: Int): ConnectionIO[GmosNorth] =
      for {
        ns <- Statements.Gmos.selectNodAndShuffle(sid, GmosN).option
        gn <- Statements.Gmos.selectNorth(sid).unique
      } yield GmosNorth.NodAndShuffle.set(gn, ns)

    def selectSouth(sid: Int): ConnectionIO[GmosSouth] =
      for {
        ns <- Statements.Gmos.selectNodAndShuffle(sid, GmosS).option
        gs <- Statements.Gmos.selectSouth(sid).unique
      } yield GmosSouth.NodAndShuffle.set(gs, ns)
  }

  object Statements {
    def insertBaseSlice(i: Instrument): Update0 =
      sql"""
        INSERT INTO static_config (instrument)
        VALUES ($i)
      """.update

    /** F2 Statements. */
    object F2 {

      import Instrument.Flamingos2

      def select(sid: Int): Query0[StaticConfig.F2] =
        sql"""
          SELECT mos_preimaging
            FROM static_f2
           WHERE static_id = $sid AND instrument = ${Flamingos2: Instrument}
        """.query[StaticConfig.F2]

      def insert(id: Int, f2: StaticConfig.F2): Update0 =
        sql"""
          INSERT INTO static_f2 (static_id, instrument, mos_preimaging)
          VALUES (
            $id,
            ${Flamingos2: Instrument},
            ${f2.mosPreImaging})
        """.update
    }

    /** GMOS Statements. */
    object Gmos {

      import gem.config.Gmos.{ GmosCommonStaticConfig => GmosCommonSC, GmosNodAndShuffle, GmosShuffleCycles, GmosShuffleOffset }
      import gem.enum.Instrument.{ GmosN, GmosS }
      import StaticConfig.{ GmosNorth, GmosSouth }

      // We need to define this explicitly because we're ignoring the nod and
      // shuffle bit.
      implicit val GmosCommonStaticComposite: Composite[GmosCommonSC] =
        Composite[(GmosDetector, MosPreImaging)].xmap(
          (t: (GmosDetector, MosPreImaging)) => GmosCommonSC(t._1, t._2, None),
          (s: GmosCommonSC) => (s.detector, s.mosPreImaging)
        )

      implicit val MetaGmosShuffleOffset: Meta[GmosShuffleOffset] =
        Meta[Int].xmap(GmosShuffleOffset.unsafeFromRowCount, _.detectorRows)

      implicit val MetaGmosShuffleCycles: Meta[GmosShuffleCycles] =
        Meta[Int].xmap(GmosShuffleCycles.unsafeFromCycleCount, _.toInt)

      def selectNodAndShuffle(sid: Int, i: Instrument): Query0[GmosNodAndShuffle] =
        sql"""
          SELECT a_offset_p,
                 a_offset_q,
                 b_offset_p,
                 b_offset_q,
                 e_offset,
                 offset_rows,
                 cycles
            FROM gmos_nod_and_shuffle
           WHERE static_id = $sid AND instrument = $i
        """.query[GmosNodAndShuffle]

      def selectNorth(sid: Int): Query0[GmosNorth] =
        sql"""
          SELECT detector,
                 mos_preimaging,
                 stage_mode
            FROM static_gmos_north
           WHERE static_id = $sid AND instrument = ${GmosN: Instrument}
        """.query[GmosNorth]

      def selectSouth(sid: Int): Query0[GmosSouth] =
        sql"""
          SELECT detector,
                 mos_preimaging,
                 stage_mode
            FROM static_gmos_south
           WHERE static_id = $sid AND instrument = ${GmosS: Instrument}
        """.query[GmosSouth]

      def insertNodAndShuffle(id: Int, inst: Instrument, ns: GmosNodAndShuffle): Update0 =
        sql"""
          INSERT INTO gmos_nod_and_shuffle (
                static_id,
                instrument,
                a_offset_p,
                a_offset_q,
                b_offset_p,
                b_offset_q,
                e_offset,
                offset_rows,
                cycles)
         VALUES (
              $id,
              $inst,
              ${ns.posA.p},
              ${ns.posA.q},
              ${ns.posB.p},
              ${ns.posB.q},
              ${ns.eOffset},
              ${ns.shuffle},
              ${ns.cycles})
        """.update

      def insertNorth(id: Int, g: GmosNorth): Update0 =
        sql"""
          INSERT INTO static_gmos_north (static_id, instrument, detector, mos_preimaging, stage_mode)
          VALUES (
            $id,
            ${GmosN: Instrument},
            ${g.common.detector},
            ${g.common.mosPreImaging},
            ${g.stageMode})
        """.update

      def insertSouth(id: Int, g: GmosSouth): Update0 =
        sql"""
          INSERT INTO static_gmos_south (static_id, instrument, detector, mos_preimaging, stage_mode)
          VALUES (
            $id,
            ${GmosS: Instrument},
            ${g.common.detector},
            ${g.common.mosPreImaging},
            ${g.stageMode})
        """.update

    }

  }

}
