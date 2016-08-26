trait SmartCalDescribe {
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
