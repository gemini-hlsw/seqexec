package gem

import gem.config._

package object enum {

  /** Add mapping from InstrumentConfig to Instrument. */
  implicit class InstrumentCompanionOps(companion: Instrument.type) {
    def forConfig(c: InstrumentConfig): Instrument =
      c match {
        case F2Config(_, _, _, _, _, _) => Instrument.Flamingos2
        case GenericConfig(i)           => i
      }
  }

  /** Add mapping from Step to StepType. */
  implicit class StepTypeCompanionOps(companion: StepType.type) {
    def forStep(s: Step[_]): StepType =
      s match {
        case BiasStep(_)        => StepType.Bias
        case DarkStep(_)        => StepType.Dark
        case GcalStep(_, _)     => StepType.Gcal
        case ScienceStep(_, _)  => StepType.Science
        case SmartStep(_, _)    => StepType.Smart
      }
  }

}
