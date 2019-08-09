// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Eq
import cats.implicits._
import seqexec.server.gems.Gems.GemsWfsState
import seqexec.server.tcs.Gaos.{PauseConditionSet, PauseResume, ResumeConditionSet}
import squants.Time

trait GemsController[F[_]] {
  import GemsController._

  def pauseResume(config: GemsConfig, pauseReasons: PauseConditionSet,
                  resumeReasons: ResumeConditionSet): F[PauseResume[F]]
  def observe(expTime: Time): F[Unit]
  def endObserve: F[Unit]

  val stateGetter: GemsWfsState[F]

}

object GemsController {

  sealed trait P1Usage extends Product with Serializable
  object P1Usage {
    case object UseP1 extends P1Usage
    case object DontUseP1 extends P1Usage

    implicit val p1UsageEq: Eq[P1Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): P1Usage = if(b) UseP1 else DontUseP1
  }

  sealed trait OIUsage extends Product with Serializable
  object OIUsage {
    case object UseOI extends OIUsage
    case object DontUseOI extends OIUsage

    implicit val oiUsageEq: Eq[OIUsage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): OIUsage = if(b) UseOI else DontUseOI
  }

  sealed trait GemsConfig extends Product with Serializable {
    val isP1Used: Boolean
    val isOIUsed: Boolean
  }

  case object GemsOff extends GemsConfig {
    override val isP1Used: Boolean = false
    override val isOIUsed: Boolean = false
  }

  sealed trait Ttgs1Usage extends Product with Serializable
  object Ttgs1Usage {
    case object UseTtgs1 extends Ttgs1Usage
    case object DontUseTtgs1 extends Ttgs1Usage

    implicit val ttgs1UsageEq: Eq[Ttgs1Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Ttgs1Usage = if(b) UseTtgs1 else DontUseTtgs1
  }

  sealed trait Ttgs2Usage extends Product with Serializable
  object Ttgs2Usage {
    case object UseTtgs2 extends Ttgs2Usage
    case object DontUseTtgs2 extends Ttgs2Usage

    implicit val ttgs2UsageEq: Eq[Ttgs2Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Ttgs2Usage = if(b) UseTtgs2 else DontUseTtgs2
  }

  sealed trait Ttgs3Usage extends Product with Serializable
  object Ttgs3Usage {
    case object UseTtgs3 extends Ttgs3Usage
    case object DontUseTtgs3 extends Ttgs3Usage

    implicit val ttgs3UsageEq: Eq[Ttgs3Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Ttgs3Usage = if(b) UseTtgs3 else DontUseTtgs3
  }

  sealed trait Odgw1Usage extends Product with Serializable
  object Odgw1Usage {
    case object UseOdgw1 extends Odgw1Usage
    case object DontUseOdgw1 extends Odgw1Usage

    implicit val odgw1UsageEq: Eq[Odgw1Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Odgw1Usage = if(b) UseOdgw1 else DontUseOdgw1
  }

  sealed trait Odgw2Usage extends Product with Serializable
  object Odgw2Usage {
    case object UseOdgw2 extends Odgw2Usage
    case object DontUseOdgw2 extends Odgw2Usage

    implicit val odgw2UsageEq: Eq[Odgw2Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Odgw2Usage = if(b) UseOdgw2 else DontUseOdgw2
  }

  sealed trait Odgw3Usage extends Product with Serializable
  object Odgw3Usage {
    case object UseOdgw3 extends Odgw3Usage
    case object DontUseOdgw3 extends Odgw3Usage

    implicit val odgw3UsageEq: Eq[Odgw3Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Odgw3Usage = if(b) UseOdgw3 else DontUseOdgw3
  }

  sealed trait Odgw4Usage extends Product with Serializable
  object Odgw4Usage {
    case object UseOdgw4 extends Odgw4Usage
    case object DontUseOdgw4 extends Odgw4Usage

    implicit val odgw4UsageEq: Eq[Odgw4Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Odgw4Usage = if(b) UseOdgw4 else DontUseOdgw4
  }

  final case class GemsOn(
                           ttgs1: Ttgs1Usage,
                           ttgs2: Ttgs2Usage,
                           ttgs3: Ttgs3Usage,
                           odgw1: Odgw1Usage,
                           odgw2: Odgw2Usage,
                           odgw3: Odgw3Usage,
                           odgw4: Odgw4Usage,
                           useP1: P1Usage,
                           useOI: OIUsage
  ) extends GemsConfig {

    import P1Usage._
    override val isP1Used: Boolean = useP1 === UseP1

    import OIUsage._
    override val isOIUsed: Boolean = useOI === UseOI
  }

}
