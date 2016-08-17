package gem
package config

import gem.describe._
import gem.describe.Metadata.Access.Science
import gem.describe.Metadata.{Attrs, Label}
import gem.describe.Metadata.Scope.SingleStep

import gem.enum._

import scalaz._, Scalaz._

case class GcalConfig(lamp: GCalLamp, shutter: GCalShutter)

object GcalConfig {
  import EnumMetadata.forEnumerated

  val Lab = Label("GCal Unit")

  def lab(name: String): Label = Label(Lab, name)

  object LampProp extends Prop[GcalConfig] {
    type B = GCalLamp
    val eq: Equal[GCalLamp]    = Equal.equalA
    val lens: GcalConfig @> GCalLamp = Lens.lensu((a,b) => a.copy(lamp = b), _.lamp)

    val meta = forEnumerated[GCalLamp](Attrs(lab("Lamp"), Science, SingleStep))
  }


  object ShutterProp extends Prop[GcalConfig] {
    type B = GCalShutter
    val eq: Equal[GCalShutter]    = Equal.equalA
    val lens: GcalConfig @> GCalShutter = Lens.lensu((a,b) => a.copy(shutter = b), _.shutter)

    val meta = forEnumerated[GCalShutter](Attrs(lab("Shutter"), Science, SingleStep))
  }

  implicit val DescribeGcal: Describe[GcalConfig] =
    Describe.forProps(
      GcalConfig(GCalLamp.IrGreyBodyHigh, GCalShutter.Open),
      LampProp, ShutterProp
    )
}