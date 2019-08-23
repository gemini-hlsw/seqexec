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
    case object Use extends P1Usage
    case object DontUse extends P1Usage

    implicit val p1UsageEq: Eq[P1Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): P1Usage = if(b) Use else DontUse
  }

  sealed trait OIUsage extends Product with Serializable
  object OIUsage {
    case object Use extends OIUsage
    case object DontUse extends OIUsage

    implicit val oiUsageEq: Eq[OIUsage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): OIUsage = if(b) Use else DontUse
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
    case object Use extends Cwfs1Usage
    case object DontUse extends Cwfs1Usage

    implicit val cwfs1UsageEq: Eq[Cwfs1Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Cwfs1Usage = if(b) Use else DontUse
  }

  sealed trait Cwfs2Usage extends Product with Serializable
  object Cwfs2Usage {
    case object Use extends Cwfs2Usage
    case object DontUse extends Cwfs2Usage

    implicit val cwfs2UsageEq: Eq[Cwfs2Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Cwfs2Usage = if(b) Use else DontUse
  }

  sealed trait Cwfs3Usage extends Product with Serializable
  object Cwfs3Usage {
    case object Use extends Cwfs3Usage
    case object DontUse extends Cwfs3Usage

    implicit val cwfs3UsageEq: Eq[Cwfs3Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Cwfs3Usage = if(b) Use else DontUse
  }

  sealed trait Odgw1Usage extends Product with Serializable
  object Odgw1Usage {
    case object Use extends Odgw1Usage
    case object DontUse extends Odgw1Usage

    implicit val odgw1UsageEq: Eq[Odgw1Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Odgw1Usage = if(b) Use else DontUse
  }

  sealed trait Odgw2Usage extends Product with Serializable
  object Odgw2Usage {
    case object Use extends Odgw2Usage
    case object DontUse extends Odgw2Usage

    implicit val odgw2UsageEq: Eq[Odgw2Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Odgw2Usage = if(b) Use else DontUse
  }

  sealed trait Odgw3Usage extends Product with Serializable
  object Odgw3Usage {
    case object Use extends Odgw3Usage
    case object DontUse extends Odgw3Usage

    implicit val odgw3UsageEq: Eq[Odgw3Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Odgw3Usage = if(b) Use else DontUse
  }

  sealed trait Odgw4Usage extends Product with Serializable
  object Odgw4Usage {
    case object Use extends Odgw4Usage
    case object DontUse extends Odgw4Usage

    implicit val odgw4UsageEq: Eq[Odgw4Usage] = Eq.fromUniversalEquals

    def fromBoolean(b: Boolean): Odgw4Usage = if(b) Use else DontUse
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

    import Cwfs1Usage.cwfs1UsageEq
    override val isCwfs1Used: Boolean = cwfs1 === Cwfs1Usage.Use

    import Cwfs2Usage.cwfs2UsageEq
    override val isCwfs2Used: Boolean = cwfs2 === Cwfs2Usage.Use

    import Cwfs3Usage.cwfs3UsageEq
    override val isCwfs3Used: Boolean = cwfs3 === Cwfs3Usage.Use

    import Odgw1Usage.odgw1UsageEq
    override val isOdgw1Used: Boolean = odgw1 === Odgw1Usage.Use

    import Odgw2Usage.odgw2UsageEq
    override val isOdgw2Used: Boolean = odgw2 === Odgw2Usage.Use

    import Odgw3Usage.odgw3UsageEq
    override val isOdgw3Used: Boolean = odgw3 === Odgw3Usage.Use

    import Odgw4Usage.odgw4UsageEq
    override val isOdgw4Used: Boolean = odgw4 === Odgw4Usage.Use

    import P1Usage.p1UsageEq
    override val isP1Used: Boolean = useP1 === P1Usage.Use

    import OIUsage.oiUsageEq
    override val isOIUsed: Boolean = useOI === OIUsage.Use
  }

}
