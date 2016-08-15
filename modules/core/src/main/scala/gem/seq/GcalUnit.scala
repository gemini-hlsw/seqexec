package gem
package seq

import gem.enum.{ GCalLamp, GCalShutter }

import gem.seq.Metadata.Access.Science
import gem.seq.Metadata.{Attrs, Label}
import gem.seq.Metadata.Scope.SingleStep

import scalaz._, Scalaz._

case class GcalUnit(lamp: GCalLamp, shutter: GCalShutter)

object GcalUnit {
  import EnumMetadata.forEnumerated

  val Lab = Label("GCal Unit")

  def lab(name: String): Label = Label(Lab, name)

  object LampProp extends Prop[GcalUnit] {
    type B = GCalLamp
    val eq: Equal[GCalLamp]    = Equal.equalA
    val lens: GcalUnit @> GCalLamp = Lens.lensu((a,b) => a.copy(lamp = b), _.lamp)

    val meta = forEnumerated[GCalLamp](Attrs(lab("Lamp"), Science, SingleStep))
  }


  object ShutterProp extends Prop[GcalUnit] {
    type B = GCalShutter
    val eq: Equal[GCalShutter]    = Equal.equalA
    val lens: GcalUnit @> GCalShutter = Lens.lensu((a,b) => a.copy(shutter = b), _.shutter)

    val meta = forEnumerated[GCalShutter](Attrs(lab("Shutter"), Science, SingleStep))
  }

  implicit val DescribeGcal: Describe[GcalUnit] =
    Describe.forProps(
      GcalUnit(GCalLamp.IrGreyBodyHigh, GCalShutter.Open),
      LampProp, ShutterProp
    )
}