// package gem


// import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{Filter, Disperser}
// import edu.gemini.spModel.core.AngleSyntax._
// import edu.gemini.spModel.core.{OffsetQ, OffsetP}

// import java.io.{ObjectInputStream, ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream}

// import scalaz._
// import Scalaz._

// object Sample extends App {
//   def serialize[A](a: A): Array[Byte] = {
//     val baos = new ByteArrayOutputStream()
//     val oos = new ObjectOutputStream(baos)
//     oos.writeObject(a)
//     oos.close()
//     baos.toByteArray
//   }

//   def deserialize[A](a: Array[Byte]): A = {
//     val bais = new ByteArrayInputStream(a)
//     val ois  = new ObjectInputStream(bais)
//     val f2   = ois.readObject().asInstanceOf[A]
//     ois.close()
//     f2
//   }

//   val f2   = implicitly[Describe[F2]].default
//   val f2_J = f2.copy(filter = Filter.J)
//   val f2_H = f2.copy(filter = Filter.H)

//   val nod_A = Telescope(OffsetP.Zero,   40.0.arcsecs[OffsetQ])
//   val nod_B = Telescope(OffsetP.Zero, (-40.0).arcsecs[OffsetQ])

//   val steps = NonEmptyList(
//     DarkStep(    f2_J),
//     ScienceStep( f2_J, nod_A),
//     ScienceStep( f2_J, nod_B),
//     ScienceStep( f2_J, nod_B),
//     ScienceStep( f2_J, nod_A),
//     SmartStep(   f2_J, SmartCal(SmartCal.Type.Arc)),
//     ScienceStep( f2_H, nod_A),
//     ScienceStep( f2_H, nod_B),
//     ScienceStep( f2_H, nod_B),
//     ScienceStep( f2_H, nod_A),
//     SmartStep(   f2_H, SmartCal(SmartCal.Type.Flat))
//   )

//   val seq = Sequence.fromSteps(steps)

//   val steps2 = deserialize[Sequence[F2]](serialize(seq)).toSteps

//   steps2.list.foreach { step =>
//     println(step.shows)
//   }
// }
