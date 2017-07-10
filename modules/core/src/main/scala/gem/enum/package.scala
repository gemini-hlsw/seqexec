// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import java.time.Month
import java.time.Month._
import scalaz.Scalaz._

// The members of this package are generated from database tables, which are the source of truth.
// See project/gen2.scala for details. Associations with other model types, as needed, are provided
// here as implicit classes wrapping val the generated companion object extends AnyVals.
package object enum {

  /** Add mapping from Step to StepType. */
  implicit class StepTypeCompanionOps(val value: StepType.type) extends AnyVal {
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
  implicit class SmartGcalTypeOps(val value: SmartGcalType) extends AnyVal {
    def fold[X](lamp: GcalLampType => X, baseline: GcalBaselineType => X): X =
      value match {
        case SmartGcalType.Arc           => lamp(GcalLampType.Arc)
        case SmartGcalType.Flat          => lamp(GcalLampType.Flat)
        case SmartGcalType.DayBaseline   => baseline(GcalBaselineType.Day)
        case SmartGcalType.NightBaseline => baseline(GcalBaselineType.Night)
      }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  implicit class HalfCompanionOps(val value: Half.type) extends AnyVal {

    def unsafeFromInt(n: Int): Half =
      fromInt(n).getOrElse(throw new NoSuchElementException(n.toString))

    def fromInt(n: Int): Option[Half] =
      value.all.find(_.toInt === n)

    def fromMonth(m: Month): Half =
      m match {
        case FEBRUARY | MARCH     | APRIL   | MAY      | JUNE     | JULY    => Half.A
        case AUGUST   | SEPTEMBER | OCTOBER | NOVEMBER | DECEMBER | JANUARY => Half.B
      }

  }

  implicit class HalfOps(val value: Half) extends AnyVal {

    def startMonth: Month =
      value match {
        case Half.A => FEBRUARY
        case Half.B => AUGUST
      }

    def endMonth: Month =
      value match {
        case Half.A => JULY
        case Half.B => JANUARY
      }

  }

}
