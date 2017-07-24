package engine

/**
  * A Seqexec resource represents any system that can be only used by one single agent.
  *
  */
sealed trait Resource
object Resource {

  case object P1 extends Resource
  case object OI extends Resource
  // Mount and science fold cannot be controlled independently. Maybe in the future.
  // For now, I replaced them with TCS
  //  case object Mount extends Resource
  //  case object ScienceFold extends Resource
  case object TCS extends Resource
  case object Gcal extends Resource
  case object Gems extends Resource
  case object Altair extends Resource

  trait Instrument extends Resource
  object Instrument {
    case object GMOS extends Instrument
    case object F2 extends Instrument
    case object GSAOI extends Instrument
    case object GPI extends Instrument
    case object NIRI extends Instrument
    case object NIFS extends Instrument
    case object GNIRS extends Instrument
  }

}
