package gem

import gem.config._

// The members of this package are generated from database tables, which are the source of truth.
// See project/gen2.scala for details. Associations with other model types, as needed, are provided
// here as implicit classes wrapping the generated companion objects.
package object enum {

  /** Add mapping from InstrumentConfig to Instrument. */
  implicit class InstrumentCompanionOps(companion: Instrument.type) {
    def forConfig(c: InstrumentConfig): Instrument =
      c match {
        case F2Config(_, _, _, _, _, _, _) => Instrument.Flamingos2
        case GenericConfig(i)              => i
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
