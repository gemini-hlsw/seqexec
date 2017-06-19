// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie.imports._

import gem.enum.Instrument
import gem.config._

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
      case _: AcqCamStaticConfig   => 0.point[ConnectionIO]
      case _: BhrosStaticConfig    => 0.point[ConnectionIO]
      case f2: F2StaticConfig      => Statements.insertF2(id, f2).run
      case _: GmosNStaticConfig    => 0.point[ConnectionIO]
      case _: GmosSStaticConfig    => 0.point[ConnectionIO]
      case _: GnirsStaticConfig    => 0.point[ConnectionIO]
      case _: GpiStaticConfig      => 0.point[ConnectionIO]
      case _: GsaoiStaticConfig    => 0.point[ConnectionIO]
      case _: MichelleStaticConfig => 0.point[ConnectionIO]
      case _: NiciStaticConfig     => 0.point[ConnectionIO]
      case _: NifsStaticConfig     => 0.point[ConnectionIO]
      case _: NiriStaticConfig     => 0.point[ConnectionIO]
      case _: PhoenixStaticConfig  => 0.point[ConnectionIO]
      case _: TrecsStaticConfig    => 0.point[ConnectionIO]
      case _: VisitorStaticConfig  => 0.point[ConnectionIO]
    }

  def select(i: Instrument, sid: Int): ConnectionIO[StaticConfig] = {
    def point(sc: StaticConfig): ConnectionIO[StaticConfig] =
      sc.point[ConnectionIO]

    i match {
      case Instrument.AcqCam     => point(AcqCamStaticConfig())
      case Instrument.Bhros      => point(BhrosStaticConfig())

      case Instrument.Flamingos2 => Statements.selectF2(sid).unique.widen[StaticConfig]

      case Instrument.GmosN      => point(GmosNStaticConfig())
      case Instrument.GmosS      => point(GmosSStaticConfig())
      case Instrument.Gnirs      => point(GnirsStaticConfig())
      case Instrument.Gpi        => point(GpiStaticConfig())
      case Instrument.Gsaoi      => point(GsaoiStaticConfig())
      case Instrument.Michelle   => point(MichelleStaticConfig())
      case Instrument.Nici       => point(NiciStaticConfig())
      case Instrument.Nifs       => point(NifsStaticConfig())
      case Instrument.Niri       => point(NiriStaticConfig())
      case Instrument.Phoenix    => point(PhoenixStaticConfig())
      case Instrument.Trecs      => point(TrecsStaticConfig())
      case Instrument.Visitor    => point(VisitorStaticConfig())
    }
  }


  object Statements {
    def selectF2(sid: Int): Query0[F2StaticConfig] =
      sql"""
        SELECT mos_preimaging
          FROM static_f2
         WHERE static_id = $sid AND instrument = ${Instrument.Flamingos2: Instrument}
      """.query[F2StaticConfig]

    def insertBaseSlice(i: Instrument): Update0 =
      sql"""
        INSERT INTO static_config (instrument)
        VALUES ($i)
      """.update

    def insertF2(id: Int, f2: F2StaticConfig): Update0 =
      sql"""
        INSERT INTO static_f2 (static_id, instrument, mos_preimaging)
        VALUES (
          $id,
          ${Instrument.Flamingos2: Instrument},
          ${f2.mosPreImaging})
      """.update
  }

}
