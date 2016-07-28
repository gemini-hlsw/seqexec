// package gem.seq

// import edu.gemini.spModel.gemini.calunit.CalUnitParams.{Lamp, Shutter}
// import gem.seq.Metadata.Access.Science
// import gem.seq.Metadata.{Attrs, Label}
// import gem.seq.Metadata.Scope.SingleStep

// import scalaz._, Scalaz._

// case class GcalUnit(lamp: Lamp, shutter: Shutter)

// object GcalUnit {
//   import EnumMetadata.fromJava

//   val Lab = Label("GCal Unit")

//   def lab(name: String): Label = Label(Lab, name)

//   object LampProp extends Prop[GcalUnit] {
//     type B = Lamp
//     val eq: Equal[Lamp]    = Equal.equalA
//     val lens: GcalUnit @> Lamp = Lens.lensu((a,b) => a.copy(lamp = b), _.lamp)

//     val meta = fromJava(Attrs(lab("Lamp"), Science, SingleStep), classOf[Lamp])
//   }


//   object ShutterProp extends Prop[GcalUnit] {
//     type B = Shutter
//     val eq: Equal[Shutter]    = Equal.equalA
//     val lens: GcalUnit @> Shutter = Lens.lensu((a,b) => a.copy(shutter = b), _.shutter)

//     val meta = fromJava(Attrs(lab("Shutter"), Science, SingleStep), classOf[Shutter])
//   }

//   implicit val DescribeGcal: Describe[GcalUnit] =
//     Describe.forProps(
//       GcalUnit(Lamp.DEFAULT, Shutter.DEFAULT),
//       LampProp, ShutterProp
//     )
// }