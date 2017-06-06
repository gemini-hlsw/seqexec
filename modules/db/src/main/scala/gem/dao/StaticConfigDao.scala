package gem
package dao

import doobie.imports._

import gem.enum.Instrument
import gem.config._

import scalaz._
import Scalaz._


object StaticConfigDao {

  /*
  // this is just a placeholder … right now the obs table just contains an instrument tag which we
  // use to construct an empty static config

  def forInstrument(i: Instrument): StaticConfig.Aux[i.type] =
    (i match {
      case Instrument.Phoenix    => PhoenixStaticConfig()
      case Instrument.Michelle   => MichelleStaticConfig()
      case Instrument.Gnirs      => GnirsStaticConfig()
      case Instrument.Niri       => NiriStaticConfig()
      case Instrument.Trecs      => TrecsStaticConfig()
      case Instrument.Nici       => NiciStaticConfig()
      case Instrument.Nifs       => NifsStaticConfig()
      case Instrument.Gpi        => GpiStaticConfig()
      case Instrument.Gsaoi      => GsaoiStaticConfig()
      case Instrument.GmosS      => GmosSStaticConfig()
      case Instrument.AcqCam     => AcqCamStaticConfig()
      case Instrument.GmosN      => GmosNStaticConfig()
      case Instrument.Bhros      => BhrosStaticConfig()
      case Instrument.Visitor    => VisitorStaticConfig()
      case Instrument.Flamingos2 => Flamingos2StaticConfig(false) // TODO
    }).asInstanceOf[StaticConfig.Aux[i.type]] // Scala isn't smart enough to know this
    */

  def insert(s: StaticConfig): ConnectionIO[Int] =
    for {
      id <- Statements.insertBaseSlice(s.instrument).withUniqueGeneratedKeys[Int]("static_id")
      _  <- insertConfigSlice(id, s)
    } yield id

  private def insertConfigSlice(id: Int, s: StaticConfig): ConnectionIO[Int] =
    s match {
      case f2: Flamingos2StaticConfig => Statements.insertF2(id, f2).run
    }

  def select(i: Instrument, sid: Int): ConnectionIO[StaticConfig] =
    i match {
      case Instrument.Flamingos2 => Statements.selectF2(sid).unique.widen[StaticConfig]
    }

  object Statements {
    def selectF2(sid: Int): Query0[Flamingos2StaticConfig] =
      sql"""
        SELECT mos_preimaging
          FROM static_f2
         WHERE static_id = $sid AND instrument = ${Instrument.Flamingos2: Instrument}
      """.query[Flamingos2StaticConfig]

//    def selectF2(oid: Observation.Id): Query0[Flamingos2StaticConfig] =
//      sql"""
//        SELECT i.mos_preimaging
//          FROM static_f2 i
//               INNER JOIN observation o
//                  ON o.static_id  = i.static_id AND
//                     o.instrument = i.instrument
//         WHERE o.observation_id = ${oid}
//      """.query[Flamingos2StaticConfig]

    def insertBaseSlice(i: Instrument): Update0 =
      sql"""
        INSERT INTO static_config (instrument)
        VALUES ($i)
      """.update

    def insertF2(id: Int, f2: Flamingos2StaticConfig): Update0 =
      sql"""
        INSERT INTO static_f2 (static_id, instrument, mos_preimaging)
        VALUES (
          $id,
          ${Instrument.Flamingos2: Instrument},
          ${f2.mosPreImaging})
      """.update
  }

}
