// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Eq
import cats.implicits._
import seqexec.server.gems.Gems.GemsGuiderStatus
import seqexec.server.tcs.Gaos.{PauseConditionSet, PauseResume, ResumeConditionSet}
import squants.Time

trait GemsController[F[_]] {
  import GemsController._

  def pauseResume(config: GemsConfig, pauseReasons: PauseConditionSet,
                  resumeReasons: ResumeConditionSet): F[PauseResume[F]]
  def observe(expTime: Time): F[Unit]
  def endObserve: F[Unit]

  val stateGetter: GemsGuiderStatus[F]

}

object GemsController {

  sealed trait P1Usage
  object P1Usage {
    case object UsesP1 extends P1Usage
    case object DoesNotUseP1 extends P1Usage

    implicit val p1UsageEq: Eq[P1Usage] = Eq.instance{
      case (UsesP1, UsesP1)             => true
      case (DoesNotUseP1, DoesNotUseP1) => true
      case _                            => false
    }
  }

  sealed trait OIUsage
  object OIUsage {
    case object UsesOI extends OIUsage
    case object DoesNotUseOI extends OIUsage

    implicit val oiUsageEq: Eq[OIUsage] = Eq.instance{
      case (UsesOI, UsesOI)             => true
      case (DoesNotUseOI, DoesNotUseOI) => true
      case _                            => false
    }
  }

  sealed trait GemsConfig {
    val usesP1: Boolean
    val usesOI: Boolean
  }

  case object GemsOff extends GemsConfig {
    override val usesP1: Boolean = false
    override val usesOI: Boolean = false
  }

  sealed trait Ttgs1Active
  object Ttgs1Active {
    case object Ttgs1On extends Ttgs1Active
    case object Ttgs1Off extends Ttgs1Active
  }

  sealed trait Ttgs2Active
  object Ttgs2Active {
    case object Ttgs2On extends Ttgs2Active
    case object Ttgs2Off extends Ttgs2Active
  }

  sealed trait Ttgs3Active
  object Ttgs3Active {
    case object Ttgs3On extends Ttgs3Active
    case object Ttgs3Off extends Ttgs3Active
  }

  sealed trait Odgw1Active
  object Odgw1Active {
    case object Odgw1On extends Odgw1Active
    case object Odgw1Off extends Odgw1Active
  }

  sealed trait Odgw2Active
  object Odgw2Active {
    case object Odgw2On extends Odgw2Active
    case object Odgw2Off extends Odgw2Active
  }

  sealed trait Odgw3Active
  object Odgw3Active {
    case object Odgw3On extends Odgw3Active
    case object Odgw3Off extends Odgw3Active
  }

  sealed trait Odgw4Active
  object Odgw4Active {
    case object Odgw4On extends Odgw4Active
    case object Odgw4Off extends Odgw4Active
  }

  import P1Usage._
  import OIUsage._

  final case class GemsOn(
    ttgs1: Ttgs1Active,
    ttgs2: Ttgs2Active,
    ttgs3: Ttgs3Active,
    odgw1: Odgw1Active,
    odgw2: Odgw2Active,
    odgw3: Odgw3Active,
    odgw4: Odgw4Active,
    useP1: P1Usage,
    useOI: OIUsage
  ) extends GemsConfig {
    override val usesP1: Boolean = useP1 === UsesP1
    override val usesOI: Boolean = useOI === UsesOI
  }

}
