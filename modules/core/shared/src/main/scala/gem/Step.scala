// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import gem.config._
import gem.enum.SmartGcalType

sealed trait Step {
  def dynamicConfig: DynamicConfig
  def base: Step.Base
}

object Step {

  // TODO: better name for this
  sealed trait Base
  object Base {
    final case object Bias extends Base
    final case object Dark extends Base
    final case class  Gcal(gcal: GcalConfig) extends Base
    final case class  Science(telescope: TelescopeConfig) extends Base
    final case class  SmartGcal(smartGcalType: SmartGcalType) extends Base
  }

  sealed case class Phoenix(dynamicConfig: DynamicConfig.Phoenix, base: Base) extends Step
  sealed case class Michelle(dynamicConfig: DynamicConfig.Michelle, base: Base) extends Step
  sealed case class Gnirs(dynamicConfig: DynamicConfig.Gnirs, base: Base) extends Step
  sealed case class Niri(dynamicConfig: DynamicConfig.Niri, base: Base) extends Step
  sealed case class Trecs(dynamicConfig: DynamicConfig.Trecs, base: Base) extends Step
  sealed case class Nici(dynamicConfig: DynamicConfig.Nici, base: Base) extends Step
  sealed case class Nifs(dynamicConfig: DynamicConfig.Nifs, base: Base) extends Step
  sealed case class Gpi(dynamicConfig: DynamicConfig.Gpi, base: Base) extends Step
  sealed case class Gsaoi(dynamicConfig: DynamicConfig.Gsaoi, base: Base) extends Step
  sealed case class GmosS(dynamicConfig: DynamicConfig.GmosS, base: Base) extends Step
  sealed case class AcqCam(dynamicConfig: DynamicConfig.AcqCam, base: Base) extends Step
  sealed case class GmosN(dynamicConfig: DynamicConfig.GmosN, base: Base) extends Step
  sealed case class Bhros(dynamicConfig: DynamicConfig.Bhros, base: Base) extends Step
  sealed case class Visitor(dynamicConfig: DynamicConfig.Visitor, base: Base) extends Step
  sealed case class Flamingos2(dynamicConfig: DynamicConfig.Flamingos2, base: Base) extends Step
  sealed case class Ghost(dynamicConfig: DynamicConfig.Ghost, base: Base) extends Step

  implicit val EqStep: Eq[Step] =
    Eq.fromUniversalEquals

}