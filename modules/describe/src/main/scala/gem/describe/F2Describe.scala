trait F2Describe {

  val Lab = Label("F2")

  private def lab(name: String): Label = Label(Lab, name)

  import BooleanMetadata.forBoolean
  import EnumMetadata.forEnumerated

  object FocalPlaneUnitProp extends Prop[F2Config] {
    type B = F2FpUnit
    val eq: Equal[F2FpUnit]  = implicitly
    val lens: F2Config @> F2FpUnit = Lens.lensu((a,b) => a.copy(fpu = b), _.fpu)
    val meta = forEnumerated[F2FpUnit](Attrs(lab("Focal Plane Unit"), Science, SingleStep))
  }

  object MosPreimagingProp extends Prop[F2Config] {
    type B = Boolean
    val eq: Equal[Boolean]  = implicitly
    val lens: F2Config @> Boolean = Lens.lensu((a,b) => a.copy(mosPreimaging = b), _.mosPreimaging)
    val meta = forBoolean(Attrs(lab("MOS pre-imaging"), Science, Global))
  }

  object ExposureTimeProp extends Prop[F2Config] {
    type B = Duration
    val eq: Equal[Duration]  = Equal.equalA
    val lens: F2Config @> Duration = Lens.lensu((a,b) => a.copy(exposureTime = b), _.exposureTime)

    val meta = TextMetadata[Duration](
      Attrs(lab("Exposure Time"), Science, SingleStep),
      Some("sec"),
      et => f"${et.toMillis/1000.0}%3.01f",
      doubleParser("Exposure Time", _)(d => Duration.ofMillis(math.round(d * 1000)))
    )
  }

  object FilterProp extends Prop[F2Config] {
    type B = F2Filter
    val eq: Equal[F2Filter]  = implicitly
    val lens: F2Config @> F2Filter = Lens.lensu((a,b) => a.copy(F2filter = b), _.F2filter)
    val meta = forEnumerated[F2Filter](Attrs(lab("F2Filter"), Science, SingleStep))
  }

  object LyotWheelProp extends Prop[F2Config] {
    type B = F2LyotWheel
    val eq: Equal[F2LyotWheel]  = implicitly
    val lens: F2Config @> F2LyotWheel = Lens.lensu((a,b) => a.copy(lyotWheel = b), _.lyotWheel)
    val meta = forEnumerated[F2LyotWheel](Attrs(lab("Lyot Wheel"), Science, SingleStep))
  }

  object DisperserProp extends Prop[F2Config] {
    type B = F2Disperser
    val eq: Equal[F2Disperser]  = implicitly
    val lens: F2Config @> F2Disperser = Lens.lensu((a,b) => a.copy(disperser = b), _.disperser)
    val meta = forEnumerated[F2Disperser](Attrs(lab("Disperser"), Science, SingleStep))
  }

  implicit val DescribeF2: Describe[F2Config] =
    Describe.forProps(
      F2Config(
        F2FpUnit.None,
        mosPreimaging = false,
        Duration.ofMillis(85000),
        F2Filter.Open,
        F2LyotWheel.F16,
        F2Disperser.NoDisperser
      ),
      FocalPlaneUnitProp,
      MosPreimagingProp,
      ExposureTimeProp,
      FilterProp,
      LyotWheelProp,
      DisperserProp
    )

}