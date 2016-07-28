package gem.seq

import gem.seq.Metadata.Access.Science
import gem.seq.Metadata.Scope.SingleStep
import gem.seq.Metadata.{Attrs, Label}

import scalaz._

final case class SmartCal(smartCalType: SmartCal.Type)

object SmartCal {
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

  object TypeProp extends Prop[SmartCal] {
    type B = Type
    val eq: Equal[Type] = Equal.equalA

    def lens: SmartCal @> Type = Lens.lensu((a, b) => a.copy(smartCalType = b), _.smartCalType)

    val meta = EnumMetadata[Type](Attrs(Label(Lab, "Cal Type"), Science, SingleStep), Type.All)
  }

  implicit val DescribeSmartCal: Describe[SmartCal] =
    Describe.forProps(
      SmartCal(SmartCal.Type.Default),
      TypeProp
    )
}
