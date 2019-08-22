// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Eq
import cats.implicits._
import seqexec.server.gems.Gems.GemsWfsState
import seqexec.server.tcs.Gaos.{PauseConditionSet, PauseResume, ResumeConditionSet}

trait GemsController[F[_]] {
  import GemsController._

  def pauseResume(pauseReasons: PauseConditionSet, resumeReasons: ResumeConditionSet)(cfg: GemsConfig)
  : F[PauseResume[F]]

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
    val isCwfs1Used: Boolean
    val isCwfs2Used: Boolean
    val isCwfs3Used: Boolean
    val isOdgw1Used: Boolean
    val isOdgw2Used: Boolean
    val isOdgw3Used: Boolean
    val isOdgw4Used: Boolean
    val isP1Used: Boolean
    val isOIUsed: Boolean
  }

  case object GemsOff extends GemsConfig {
    override val isCwfs1Used: Boolean = false
    override val isCwfs2Used: Boolean = false
    override val isCwfs3Used: Boolean = false
    override val isOdgw1Used: Boolean = false
    override val isOdgw2Used: Boolean = false
    override val isOdgw3Used: Boolean = false
    override val isOdgw4Used: Boolean = false
    override val isP1Used: Boolean = false
    override val isOIUsed: Boolean = false
  }

  sealed trait Cwfs1Usage extends Product with Serializable
  object Cwfs1Usage {
    case object UseCwfs1 extends Cwfs1Usage
    case object DontUseCwfs1 extends Cwfs1Usage

    implicit val cwfs1UsageEq: Eq[Cwfs1Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Cwfs1Usage = if(b) UseCwfs1 else DontUseCwfs1
  }

  sealed trait Cwfs2Usage extends Product with Serializable
  object Cwfs2Usage {
    case object UseCwfs2 extends Cwfs2Usage
    case object DontUseCwfs2 extends Cwfs2Usage

    implicit val cwfs2UsageEq: Eq[Cwfs2Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Cwfs2Usage = if(b) UseCwfs2 else DontUseCwfs2
  }

  sealed trait Cwfs3Usage extends Product with Serializable
  object Cwfs3Usage {
    case object UseCwfs3 extends Cwfs3Usage
    case object DontUseCwfs3 extends Cwfs3Usage

    implicit val cwfs3UsageEq: Eq[Cwfs3Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Cwfs3Usage = if(b) UseCwfs3 else DontUseCwfs3
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
                           cwfs1: Cwfs1Usage,
                           cwfs2: Cwfs2Usage,
                           cwfs3: Cwfs3Usage,
                           odgw1: Odgw1Usage,
                           odgw2: Odgw2Usage,
                           odgw3: Odgw3Usage,
                           odgw4: Odgw4Usage,
                           useP1: P1Usage,
                           useOI: OIUsage
  ) extends GemsConfig {

    import Cwfs1Usage._
    override val isCwfs1Used: Boolean = cwfs1 === UseCwfs1

    import Cwfs2Usage._
    override val isCwfs2Used: Boolean = cwfs2 === UseCwfs2

    import Cwfs3Usage._
    override val isCwfs3Used: Boolean = cwfs3 === UseCwfs3

    import Odgw1Usage._
    override val isOdgw1Used: Boolean = odgw1 === UseOdgw1

    import Odgw2Usage._
    override val isOdgw2Used: Boolean = odgw2 === UseOdgw2

    import Odgw3Usage._
    override val isOdgw3Used: Boolean = odgw3 === UseOdgw3

    import Odgw4Usage._
    override val isOdgw4Used: Boolean = odgw4 === UseOdgw4

    import P1Usage._
    override val isP1Used: Boolean = useP1 === UseP1

    import OIUsage._
    override val isOIUsed: Boolean = useOI === UseOI
  }

}
