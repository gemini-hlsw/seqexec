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
    def forStep(s: Step): StepType =
      s.base match {
        case Step.Base.Bias         => StepType.Bias
        case Step.Base.Dark         => StepType.Dark
        case Step.Base.Gcal(_)      => StepType.Gcal
        case Step.Base.Science(_)   => StepType.Science
        case Step.Base.SmartGcal(_) => StepType.SmartGcal
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
        case _: Asterism.SingleTarget => AsterismType.SingleTarget
        case _: Asterism.DualTarget   => AsterismType.GhostDualTarget
      }
  }

  /**
   * Enrichment methods for [[Instrument]].
   * @group Enrichment
   */
  implicit class InstrumentOps(val value: Instrument.type) extends AnyVal {
    def forStep(s: Step): Instrument =
      s match {
        case Step.Phoenix(_, _)    => Instrument.Phoenix
        case Step.Michelle(_, _)   => Instrument.Michelle
        case Step.Gnirs(_, _)      => Instrument.Gnirs
        case Step.Niri(_, _)       => Instrument.Niri
        case Step.Trecs(_, _)      => Instrument.Trecs
        case Step.Nici(_, _)       => Instrument.Nici
        case Step.Nifs(_, _)       => Instrument.Nifs
        case Step.Gpi(_, _)        => Instrument.Gpi
        case Step.Gsaoi(_, _)      => Instrument.Gsaoi
        case Step.GmosS(_, _)      => Instrument.GmosS
        case Step.AcqCam(_, _)     => Instrument.AcqCam
        case Step.GmosN(_, _)      => Instrument.GmosN
        case Step.Bhros(_, _)      => Instrument.Bhros
        case Step.Visitor(_, _)    => Instrument.Visitor
        case Step.Flamingos2(_, _) => Instrument.Flamingos2
        case Step.Ghost(_, _)      => Instrument.Ghost
      }
  }

}
