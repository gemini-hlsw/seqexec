// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie.imports._
import gem.enum.{ GmosDetector, Instrument, MosPreImaging }
import gem.config._

object StaticConfigDao {

  def insert(s: StaticConfig): ConnectionIO[Int] =
    for {
      id <- Statements.insertBaseSlice(s.instrument).withUniqueGeneratedKeys[Int]("static_id")
      _  <- insertConfigSlice(id, s)
    } yield id

  private def insertConfigSlice(id: Int, s: StaticConfig): ConnectionIO[Unit] =
    s match {
      case _:  StaticConfig.AcqCam    => ().pure[ConnectionIO]
      case _:  StaticConfig.Bhros     => ().pure[ConnectionIO]
      case f2: StaticConfig.F2        => Statements.F2.insert(id, f2).run.void
      case g:  StaticConfig.GmosNorth => Gmos.insertNorth(id, g)
      case g:  StaticConfig.GmosSouth => Gmos.insertSouth(id, g)
      case _:  StaticConfig.Gnirs     => ().pure[ConnectionIO]
      case _:  StaticConfig.Gpi       => ().pure[ConnectionIO]
      case _:  StaticConfig.Gsaoi     => ().pure[ConnectionIO]
      case _:  StaticConfig.Michelle  => ().pure[ConnectionIO]
      case _:  StaticConfig.Nici      => ().pure[ConnectionIO]
      case _:  StaticConfig.Nifs      => ().pure[ConnectionIO]
      case _:  StaticConfig.Niri      => ().pure[ConnectionIO]
      case _:  StaticConfig.Phoenix   => ().pure[ConnectionIO]
      case _:  StaticConfig.Trecs     => ().pure[ConnectionIO]
      case _:  StaticConfig.Visitor   => ().pure[ConnectionIO]
    }

  def select(i: Instrument, sid: Int): ConnectionIO[StaticConfig] = {
    def pure(sc: StaticConfig): ConnectionIO[StaticConfig] =
      sc.pure[ConnectionIO]

    i match {
      case Instrument.AcqCam     => pure(StaticConfig.AcqCam())
      case Instrument.Bhros      => pure(StaticConfig.Bhros())

      case Instrument.Flamingos2 => Statements.F2.select(sid).unique.widen[StaticConfig]
      case Instrument.GmosN      => Gmos.selectNorth(sid)           .widen[StaticConfig]
      case Instrument.GmosS      => Gmos.selectSouth(sid)           .widen[StaticConfig]

      case Instrument.Gnirs      => pure(StaticConfig.Gnirs())
      case Instrument.Gpi        => pure(StaticConfig.Gpi())
      case Instrument.Gsaoi      => pure(StaticConfig.Gsaoi())
      case Instrument.Michelle   => pure(StaticConfig.Michelle())
      case Instrument.Nici       => pure(StaticConfig.Nici())
      case Instrument.Nifs       => pure(StaticConfig.Nifs())
      case Instrument.Niri       => pure(StaticConfig.Niri())
      case Instrument.Phoenix    => pure(StaticConfig.Phoenix())
      case Instrument.Trecs      => pure(StaticConfig.Trecs())
      case Instrument.Visitor    => pure(StaticConfig.Visitor())
    }
  }

  /** Combines lower-level GMOS statements into higher-level inserts and
    * selects.
    */
  private object Gmos {

    import gem.config.GmosConfig.{ GmosCustomRoiEntry, GmosNodAndShuffle }
    import gem.enum.Instrument.{ GmosN, GmosS }
    import StaticConfig.{ GmosNorth, GmosSouth }

    def insertNorth(sid: Int, gn: GmosNorth): ConnectionIO[Unit] =
        insertNodAndShuffle(sid, GmosN, gn.common.nodAndShuffle)   *>
          insertCustomRoiEntries(sid, GmosN, gn.common.customRois) *>
          Statements.Gmos.insertNorth(sid, gn).run.void

    def insertSouth(sid: Int, gs: GmosSouth): ConnectionIO[Unit] =
        insertNodAndShuffle(sid, GmosS, gs.common.nodAndShuffle)   *>
          insertCustomRoiEntries(sid, GmosS, gs.common.customRois) *>
          Statements.Gmos.insertSouth(sid, gs).run.void

    def insertCustomRoiEntries(sid: Int, i: Instrument, rois: Set[GmosCustomRoiEntry]): ConnectionIO[Unit] =
      rois.toList.traverseU(Statements.Gmos.insertCustomRoiEntry(sid, i, _).run).void

    def insertNodAndShuffle(sid: Int, i: Instrument, ns: Option[GmosNodAndShuffle]): ConnectionIO[Unit] =
      ns.fold(().pure[ConnectionIO])(ns => Statements.Gmos.insertNodAndShuffle(sid, i, ns).run.void)

    def selectNorth(sid: Int): ConnectionIO[GmosNorth] =
      for {
        ro <- Statements.Gmos.selectCustomRoiEntry(sid, GmosN).list.map(_.toSet)
        ns <- Statements.Gmos.selectNodAndShuffle(sid, GmosN).option
        gn <- Statements.Gmos.selectNorth(sid).unique
        gnʹ = GmosNorth.CustomRois.set(gn, ro)
      } yield GmosNorth.NodAndShuffle.set(gnʹ, ns)

    def selectSouth(sid: Int): ConnectionIO[GmosSouth] =
      for {
        ro <- Statements.Gmos.selectCustomRoiEntry(sid, GmosS).list.map(_.toSet)
        ns <- Statements.Gmos.selectNodAndShuffle(sid, GmosS).option
        gs <- Statements.Gmos.selectSouth(sid).unique
        gsʹ = GmosSouth.CustomRois.set(gs, ro)
      } yield GmosSouth.NodAndShuffle.set(gsʹ, ns)
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

      import gem.config.GmosConfig.{ GmosCommonStaticConfig => GmosCommonSC, GmosCustomRoiEntry, GmosNodAndShuffle, GmosShuffleCycles, GmosShuffleOffset }
      import gem.enum.Instrument.{ GmosN, GmosS }
      import StaticConfig.{ GmosNorth, GmosSouth }

      // We need to define this explicitly because we're ignoring the nod and
      // shuffle and custom ROIs.
      implicit val GmosCommonStaticComposite: Composite[GmosCommonSC] =
        Composite[(GmosDetector, MosPreImaging)].xmap(
          (t: (GmosDetector, MosPreImaging)) => GmosCommonSC(t._1, t._2, None, Set.empty),
          (s: GmosCommonSC)                  => (s.detector, s.mosPreImaging)
        )

      implicit val MetaGmosShuffleOffset: Meta[GmosShuffleOffset] =
        Meta[Int].xmap(GmosShuffleOffset.unsafeFromRowCount, _.detectorRows)

      implicit val MetaGmosShuffleCycles: Meta[GmosShuffleCycles] =
        Meta[Int].xmap(GmosShuffleCycles.unsafeFromCycleCount, _.toInt)

      implicit val GmosCustomRoiEntryComposite: Composite[GmosCustomRoiEntry] =
        Composite[(Short, Short, Short, Short)].imap(
          (t: (Short, Short, Short, Short)) => GmosCustomRoiEntry.unsafeFromDescription(t._1, t._2, t._3, t._4))(
          (r: GmosCustomRoiEntry)           => (r.xMin, r.yMin, r.xRange, r.yRange)
        )

      def selectCustomRoiEntry(sid: Int, i: Instrument): Query0[GmosCustomRoiEntry] =
        sql"""
          SELECT x_min,
                 y_min,
                 x_range,
                 y_range
            FROM gmos_custom_roi
           WHERE static_id = $sid AND instrument = $i
         """.query[GmosCustomRoiEntry]

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

      def insertCustomRoiEntry(id: Int, inst: Instrument, roi: GmosCustomRoiEntry): Update0 =
        sql"""
          INSERT INTO gmos_custom_roi (
                        static_id,
                        instrument,
                        x_min,
                        y_min,
                        x_range,
                        y_range)
               VALUES (
                      $id,
                      $inst,
                      ${roi.xMin},
                      ${roi.yMin},
                      ${roi.xRange},
                      ${roi.yRange})
        """.update

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
