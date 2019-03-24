// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.config._
import gem.enum.SmartGcalType

import cats.Eq
import cats.implicits._
import monocle._

sealed trait Step {
  def dynamicConfig: DynamicConfig
  def base: Step.Base
}

object Step {

  // TODO: better name for this
  sealed trait Base

  object Base extends BaseOptics {

    final case object Bias extends Base
    final case object Dark extends Base
    final case class  Gcal(gcal: GcalConfig) extends Base
    final case class  Science(telescope: TelescopeConfig) extends Base
    final case class  SmartGcal(smartGcalType: SmartGcalType) extends Base

    implicit val EqBias: Eq[Bias.type] =
      Eq.allEqual

    implicit val EqDark: Eq[Dark.type] =
      Eq.allEqual

    object Gcal extends GcalOptics {

      implicit val EqGcal: Eq[Gcal] =
        Eq.by(_.gcal)

    }

    trait GcalOptics {

      /** @group Optics */
      val gcal: Lens[Gcal, GcalConfig] =
        Lens[Gcal, GcalConfig](_.gcal)(a => _.copy(gcal = a))

    }

    object Science extends ScienceOptics {

      implicit val EqScience: Eq[Science] =
        Eq.by(_.telescope)

    }

    trait ScienceOptics {

      /** @group Optics */
      val telescope: Lens[Science, TelescopeConfig] =
        Lens[Science, TelescopeConfig](_.telescope)(a => _.copy(telescope = a))

    }

    object SmartGcal extends SmartGcalOptics {

      implicit val EqSmartGcal: Eq[SmartGcal] =
        Eq.by(_.smartGcalType)

    }

    trait SmartGcalOptics {

      /** @group Optics */
      val smartGcalType: Lens[SmartGcal, SmartGcalType] =
        Lens[SmartGcal, SmartGcalType](_.smartGcalType)(a => _.copy(smartGcalType = a))

    }

    implicit val EqBase: Eq[Base] =
      Eq.instance {
        case (a: Bias.type, b: Bias.type) => a === b
        case (a: Dark.type, b: Dark.type) => a === b
        case (a: Gcal,      b: Gcal     ) => a === b
        case (a: Science,   b: Science  ) => a === b
        case (a: SmartGcal, b: SmartGcal) => a === b
        case _                            => false
      }
  }

  trait BaseOptics {

    /** @group Optics */
    val gcalConfig: Prism[Base, GcalConfig] =
      Prism.partial[Base, GcalConfig] { case Base.Gcal(c) => c }(Base.Gcal(_))

    /** @group Optics */
    val telescopeConfig: Prism[Base, TelescopeConfig] =
      Prism.partial[Base, TelescopeConfig] { case Base.Science(c) => c }(Base.Science(_))

    /** @group Optics */
    val smartGcalType: Prism[Base, SmartGcalType] =
      Prism.partial[Base, SmartGcalType] { case Base.SmartGcal(t) => t }(Base.SmartGcal(_))

  }

  sealed case class AcqCam(dynamicConfig: DynamicConfig.AcqCam, base: Base) extends Step
  sealed case class Bhros(dynamicConfig: DynamicConfig.Bhros, base: Base) extends Step
  sealed case class Flamingos2(dynamicConfig: DynamicConfig.Flamingos2, base: Base) extends Step
  sealed case class Ghost(dynamicConfig: DynamicConfig.Ghost, base: Base) extends Step
  sealed case class Gpi(dynamicConfig: DynamicConfig.Gpi, base: Base) extends Step
  sealed case class GmosN(dynamicConfig: DynamicConfig.GmosN, base: Base) extends Step
  sealed case class GmosS(dynamicConfig: DynamicConfig.GmosS, base: Base) extends Step
  sealed case class Gnirs(dynamicConfig: DynamicConfig.Gnirs, base: Base) extends Step
  sealed case class Gsaoi(dynamicConfig: DynamicConfig.Gsaoi, base: Base) extends Step
  sealed case class Michelle(dynamicConfig: DynamicConfig.Michelle, base: Base) extends Step
  sealed case class Nici(dynamicConfig: DynamicConfig.Nici, base: Base) extends Step
  sealed case class Nifs(dynamicConfig: DynamicConfig.Nifs, base: Base) extends Step
  sealed case class Niri(dynamicConfig: DynamicConfig.Niri, base: Base) extends Step
  sealed case class Phoenix(dynamicConfig: DynamicConfig.Phoenix, base: Base) extends Step
  sealed case class Trecs(dynamicConfig: DynamicConfig.Trecs, base: Base) extends Step
  sealed case class Visitor(dynamicConfig: DynamicConfig.Visitor, base: Base) extends Step


  object GmosN extends GmosNOptics

  trait GmosNOptics {

    /** @group Optics */
    val dynamicConfig: Lens[Step.GmosN, DynamicConfig.GmosN] =
      Lens[GmosN, DynamicConfig.GmosN](_.dynamicConfig)(a => _.copy(dynamicConfig = a))

    /** @group Optics */
    val base: Lens[GmosN, Base] =
      Lens[GmosN, Base](_.base)(a => _.copy(base = a))

  }

  object GmosS extends GmosSOptics

  trait GmosSOptics {

    /** @group Optics */
    val dynamicConfig: Lens[Step.GmosS, DynamicConfig.GmosS] =
      Lens[GmosS, DynamicConfig.GmosS](_.dynamicConfig)(a => _.copy(dynamicConfig = a))

    /** @group Optics */
    val base: Lens[GmosS, Base] =
      Lens[GmosS, Base](_.base)(a => _.copy(base = a))

  }

  implicit def EqStep[S <: Step]: Eq[S] =
    Eq.by(s => (s.dynamicConfig, s.base))

}