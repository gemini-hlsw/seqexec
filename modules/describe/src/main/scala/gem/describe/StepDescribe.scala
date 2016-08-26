trait StepDescribe {

  implicit def showStep[I : Describe]: Show[Step[I]] = {
    def showVals[A : Describe](name: String, a: A): String = {
      val props  = implicitly[Describe[A]].props
      val values = props.map { p =>
        val b = p.lens.get(a)
        s"${p.meta.attrs.label.name} = ${p.meta.show(b)}"
      }

      values.mkString(name + " {", ", ", "}")
    }

    Show.shows[Step[I]] {
      case BiasStep(i)       => "Bias: "      + showVals("inst", i)
      case DarkStep(i)       => "Dark: "      + showVals("inst", i)
      case GcalStep(i, g)    => "Gcal: "      + showVals("inst", i) + " " + showVals("gcal", g)
      case ScienceStep(i, t) => "Science: "   + showVals("inst", i) + " " + showVals("telescope", t)
      case SmartStep(i, t)   => s"Smart $t: " + showVals("inst", i)
    }
  }

  // --------------------------------------------------------------------------
  // Partial Lenses: Step[I] @?> A
  //
  // The presence or absence of an A for a given step will depend on the step
  // type.  For example, only Science steps have a TelescopeConfig component and only
  // Gcal steps have a GcalConfig.
  //
  // You can compose these with individual property lenses to get partial
  // lenses for instrument filters, etc.  For example:
  //
  //     Step.instrument[F2] >=> F2.FilterProp.lens.partial
  //
  // creates a partial lens:
  //
  //    Step[F2] @?> F2.FilterProp.B
  // --------------------------------------------------------------------------

  def instrument[I]: Step[I] @?> I = PLens.plensgf({
      case BiasStep(_)               => (i: I) => BiasStep(i)
      case DarkStep(_)               => (i: I) => DarkStep(i)
      case GcalStep(_, gcal)         => (i: I) => GcalStep(i, gcal)
      case ScienceStep(_, telescope) => (i: I) => ScienceStep(i, telescope)
      case SmartStep(_, smart)       => (i: I) => SmartStep(i, smart)
    }, {
      case BiasStep(i)       => i
      case DarkStep(i)       => i
      case GcalStep(i, _)    => i
      case ScienceStep(i, _) => i
      case SmartStep(i, _)   => i
    })

  def gcal[I]: Step[I] @?> GcalConfig = PLens.plensgf({
      case GcalStep(inst, _) => (g: GcalConfig) => GcalStep(inst, g)
    }, { case GcalStep(_, g) => g })

  def telescope[I]: Step[I] @?> TelescopeConfig = PLens.plensgf({
      case ScienceStep(inst,_) => (t: TelescopeConfig) => ScienceStep(inst, t)
    }, { case ScienceStep(_, t) => t })

  def smartCal[I]: Step[I] @?> SmartCalConfig = PLens.plensgf({
      case SmartStep(inst,_) => (s: SmartCalConfig) => SmartStep(inst, s)
    }, { case SmartStep(_, s) => s})
}



trait BiasStepDescribe {
  implicit def describeBiasStep[I: Describe]: Describe[BiasStep[I]] =
    Describe[I].xmap[BiasStep[I]](
      i => BiasStep(i),
      s => s.instrument
    )
}

trait DarkStepDescribe {
  implicit def describeDarkStep[I: Describe]: Describe[DarkStep[I]] =
    Describe[I].xmap[DarkStep[I]](
      i => DarkStep(i),
      s => s.instrument
    )
}

trait GcalStepDescribe {
  implicit def describeGcalStep[I: Describe]: Describe[GcalStep[I]] =
    Describe[(I, GcalConfig)].xmap[GcalStep[I]](
      t => GcalStep(t._1, t._2),
      s => (s.instrument, s.gcal)
    )
}

trait ScienceStepDescribe {
  implicit def describeScienceStep[I: Describe]: Describe[ScienceStep[I]] =
    Describe[(I, TelescopeConfig)].xmap[ScienceStep[I]](
      t => ScienceStep(t._1, t._2),
      s => (s.instrument, s.telescope)
    )
}

trait SmartStepDescribe {
  implicit def describeSmartStep[I: Describe]: Describe[SmartStep[I]] =
    Describe[(I, SmartCalConfig)].xmap[SmartStep[I]](
      t => SmartStep(t._1, t._2),
      s => (s.instrument, s.smartCal)
    )
}



