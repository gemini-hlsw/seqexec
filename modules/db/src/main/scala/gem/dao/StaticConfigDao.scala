package gem
package dao

// import doobie.imports._

import gem.enum.Instrument
import gem.config._


object StaticConfigDao {

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
      case Instrument.Flamingos2 => Flamingos2StaticConfig()
    }).asInstanceOf[StaticConfig.Aux[i.type]] // Scala isn't smart enough to know this


  object Statements {
    // todo
  }

}
