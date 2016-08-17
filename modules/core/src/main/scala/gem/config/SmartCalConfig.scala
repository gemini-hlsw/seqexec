package gem
package config

import gem.describe._
import gem.describe.Metadata.Access.Science
import gem.describe.Metadata.Scope.SingleStep
import gem.describe.Metadata.{Attrs, Label}

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

  val Lab = Label("Smart")

  object TypeProp extends Prop[SmartCalConfig] {
    type B = Type
    val eq: Equal[Type] = Equal.equalA

    def lens: SmartCalConfig @> Type = Lens.lensu((a, b) => a.copy(smartCalType = b), _.smartCalType)

    val meta = EnumMetadata[Type](Attrs(Label(Lab, "Cal Type"), Science, SingleStep), Type.All)
  }

  implicit val DescribeSmartCal: Describe[SmartCalConfig] =
    Describe.forProps(
      SmartCalConfig(SmartCalConfig.Type.Default),
      TypeProp
    )
}
