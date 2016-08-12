package gem
package seq

// // import edu.gemini.pot.sp.SPComponentType
// // import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{Disperser, Filter, FPUnit, LyotWheel}

import Metadata.Access._
import Metadata.{Attrs, Label}
import Metadata.Scope._

import java.time.Duration

import scalaz._
import Scalaz._

// final case class F2(
//     fpu: FPUnit,
//     mosPreimaging: Boolean,
//     exposureTime: Duration,
//     filter: Filter,
//     lyoutWheel: LyotWheel,
//     disperser: Disperser
//  ) extends Instrument {
//   def name = "F2"
// }

// object F2 {

//   val Lab = Label("F2")

//   private def lab(name: String): Label = Label(Lab, name)

//   import BooleanMetadata.forBoolean
//   import EnumMetadata.fromJava

//   object FocalPlaneUnitProp extends Prop[F2] {
//     type B = FPUnit
//     val eq: Equal[FPUnit]  = Equal.equalA
//     val lens: F2 @> FPUnit = Lens.lensu((a,b) => a.copy(fpu = b), _.fpu)

//     val meta = fromJava(Attrs(lab("Focal Plane Unit"), Science, SingleStep), classOf[FPUnit])
//   }

//   object MosPreimagingProp extends Prop[F2] {
//     type B = Boolean
//     val eq: Equal[Boolean]  = implicitly[Equal[Boolean]]
//     val lens: F2 @> Boolean = Lens.lensu((a,b) => a.copy(mosPreimaging = b), _.mosPreimaging)

//     val meta = forBoolean(Attrs(lab("MOS pre-imaging"), Science, Global))
//   }

//   object ExposureTimeProp extends Prop[F2] {
//     type B = Duration
//     val eq: Equal[Duration]  = Equal.equalA
//     val lens: F2 @> Duration = Lens.lensu((a,b) => a.copy(exposureTime = b), _.exposureTime)

//     val meta = TextMetadata[Duration](
//       Attrs(lab("Exposure Time"), Science, SingleStep),
//       Some("sec"),
//       et => f"${et.toMillis/1000.0}%3.01f",
//       doubleParser("Exposure Time", _)(d => Duration.ofMillis(math.round(d * 1000)))
//     )
//   }

//   object FilterProp extends Prop[F2] {
//     type B = Filter
//     val eq: Equal[Filter]  = Equal.equalA
//     val lens: F2 @> Filter = Lens.lensu((a,b) => a.copy(filter = b), _.filter)

//     val meta = fromJava(Attrs(lab("Filter"), Science, SingleStep), classOf[Filter])
//   }

//   object LyotWheelProp extends Prop[F2] {
//     type B = LyotWheel
//     val eq: Equal[LyotWheel]  = Equal.equalA
//     val lens: F2 @> LyotWheel = Lens.lensu((a,b) => a.copy(lyoutWheel = b), _.lyoutWheel)

//     val meta = fromJava(Attrs(lab("Lyot Wheel"), Science, SingleStep), classOf[LyotWheel])
//   }

//   object DisperserProp extends Prop[F2] {
//     type B = Disperser
//     val eq: Equal[Disperser]  = Equal.equalA
//     val lens: F2 @> Disperser = Lens.lensu((a,b) => a.copy(disperser = b), _.disperser)

//     val meta = fromJava(Attrs(lab("Disperser"), Science, SingleStep), classOf[Disperser])
//   }

//   implicit val DescribeF2: Describe[F2] =
//     Describe.forProps(
//       F2(
//         FPUnit.DEFAULT,
//         mosPreimaging = false,
//         Duration.ofMillis(85000),
//         Filter.DEFAULT,
//         LyotWheel.DEFAULT,
//         Disperser.DEFAULT
//       ),
//       FocalPlaneUnitProp,
//       MosPreimagingProp,
//       ExposureTimeProp,
//       FilterProp,
//       LyotWheelProp,
//       DisperserProp
//     )

// }