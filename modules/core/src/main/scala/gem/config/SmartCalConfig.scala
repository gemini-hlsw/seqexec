package gem
package config

import scalaz._

final case class SmartCalConfig(smartCalType: SmartCalConfig.Type)

object SmartCalConfig {
  sealed trait Type extends Product with Serializable
  object Type {
    case object Arc           extends Type
    case object Flat          extends Type
    case object DayBaseline   extends Type
    case object NightBaseline extends Type

    val Default = Arc
    val All     = NonEmptyList(Arc, Flat, DayBaseline, NightBaseline)
  }
}

