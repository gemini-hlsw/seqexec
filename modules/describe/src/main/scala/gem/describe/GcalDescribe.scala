trait GcalDescribe {
  import EnumMetadata.forEnumerated

  val Lab = Label("Gcal Unit")

  def lab(name: String): Label = Label(Lab, name)

  object LampProp extends Prop[GcalConfig] {
    type B = GcalLamp
    val eq: Equal[GcalLamp]    = Equal.equalA
    val lens: GcalConfig @> GcalLamp = Lens.lensu((a,b) => a.copy(lamp = b), _.lamp)

    val meta = forEnumerated[GcalLamp](Attrs(lab("Lamp"), Science, SingleStep))
  }


  object ShutterProp extends Prop[GcalConfig] {
    type B = GcalShutter
    val eq: Equal[GcalShutter]    = Equal.equalA
    val lens: GcalConfig @> GcalShutter = Lens.lensu((a,b) => a.copy(shutter = b), _.shutter)

    val meta = forEnumerated[GcalShutter](Attrs(lab("Shutter"), Science, SingleStep))
  }

  implicit val DescribeGcal: Describe[GcalConfig] =
    Describe.forProps(
      GcalConfig(GcalLamp.IrGreyBodyHigh, GcalShutter.Open),
      LampProp, ShutterProp
    )
}