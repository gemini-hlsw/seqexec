trait TelescopeDescribe {
  val Lab = Label("Telescope")

  def lab(name: String): Label = Label(Lab, name)

  object OffsetPProp extends Prop[TelescopeConfig] {
    type B = OffsetP
    val eq: Equal[OffsetP]         = Equal[OffsetP]
    val lens: TelescopeConfig @> OffsetP = Lens.lensu((a, b) => a.copy(p = b), _.p)

    val meta = new TextMetadata[OffsetP](
      Attrs(lab("p"), Science, SingleStep),
      Some("arcsec"),
      p => f"${p.arcsecs}%4.03f",
      doubleParser("Offset p", _)(_.arcsecs[OffsetP])
    )
  }

  object OffsetQProp extends Prop[TelescopeConfig] {
    type B = OffsetQ
    val eq: Equal[OffsetQ]         = Equal[OffsetQ]
    val lens: TelescopeConfig @> OffsetQ = Lens.lensu((a, b) => a.copy(q = b), _.q)

    val meta = new TextMetadata[OffsetQ](
      Attrs(lab("q"), Science, SingleStep),
      Some("arcsec"),
      q => f"${q.arcsecs}%4.03f",
      doubleParser("Offset q", _)(_.arcsecs[OffsetQ])
    )
  }

  implicit val DescribeTelescope: Describe[TelescopeConfig] =
    Describe.forProps(
      Zero,
      OffsetPProp, OffsetQProp
    )
    
}