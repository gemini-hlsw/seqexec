package gem

// The members of this package are generated from database tables, which are the source of truth.
// See project/gen2.scala for details. Associations with other model types, as needed, are provided
// here as implicit classes wrapping the generated companion objects.
package object enum {

  /** Add mapping from Step to StepType. */
  implicit class StepTypeCompanionOps(companion: StepType.type) {
    def forStep(s: Step[_]): StepType =
      s match {
        case BiasStep(_)         => StepType.Bias
        case DarkStep(_)         => StepType.Dark
        case GcalStep(_, _)      => StepType.Gcal
        case ScienceStep(_, _)   => StepType.Science
        case SmartGcalStep(_, _) => StepType.SmartGcal
      }
  }

  /** Add fold on SmartGcalType. */
  implicit class SmartGcalTypeOps(t: SmartGcalType) {
    def fold[X](lamp: GcalLampType => X, baseline: GcalBaselineType => X): X =
      t match {
        case SmartGcalType.Arc           => lamp(GcalLampType.Arc)
        case SmartGcalType.Flat          => lamp(GcalLampType.Flat)
        case SmartGcalType.DayBaseline   => baseline(GcalBaselineType.Day)
        case SmartGcalType.NightBaseline => baseline(GcalBaselineType.Night)
      }
  }
}
