// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import java.time.Month
import java.time.Month._
import cats.implicits._
import gem.syntax.ToPrismOps

// The members of this package are generated from database tables, which are the source of truth.
// See project/gen2.scala for details. Associations with other model types, as needed, are provided
// here as implicit classes wrapping val the generated companion object extends AnyVals.

/**
 * Enumerated types (normally generated from database tables) and related syntactic enrichments.
 */
package object enum extends ToPrismOps {

  /**
   * Enrichment methods for the [[StepType]] companion object.
   * @group Enrichments
   */
  implicit class StepTypeCompanionOps(val value: StepType.type) extends AnyVal {
    def forStep(s: Step[_]): StepType =
      s match {
        case Step.Bias(_)         => StepType.Bias
        case Step.Dark(_)         => StepType.Dark
        case Step.Gcal(_, _)      => StepType.Gcal
        case Step.Science(_, _)   => StepType.Science
        case Step.SmartGcal(_, _) => StepType.SmartGcal
      }
  }

  /**
   * Enrichment methods for [[SmartGcalType]].
   * @group Enrichments
   */
  implicit class SmartGcalTypeOps(val value: SmartGcalType) extends AnyVal {
    def fold[X](lamp: GcalLampType => X, baseline: GcalBaselineType => X): X =
      value match {
        case SmartGcalType.Arc           => lamp(GcalLampType.Arc)
        case SmartGcalType.Flat          => lamp(GcalLampType.Flat)
        case SmartGcalType.DayBaseline   => baseline(GcalBaselineType.Day)
        case SmartGcalType.NightBaseline => baseline(GcalBaselineType.Night)
      }
  }

  /**
   * Enrichment methods for the [[Half]] companion object.
   * @group Enrichments
   */
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

  /**
   * Enrichment methods for [[Half]].
   * @group Enrichments
   */
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

  /**
   * Enrichment methods for [[AsterismType]].
   * @group Enrichment
   */
  implicit class AsterismTypeOps(val value: AsterismType.type) extends AnyVal {
    def of(a: Asterism): AsterismType =
      a match {
        case _: Asterism.SingleTarget[_] => AsterismType.SingleTarget
        case _: Asterism.GhostDualTarget => AsterismType.GhostDualTarget
      }
  }
}
