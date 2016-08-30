// package gem
//
// import gem.enum._
//
// import edu.gemini.spModel.core._
// import scala.reflect.ClassTag
//
// import scalaz._, Scalaz._
//
// final class ConfigReader[A](run: Coyoneda[(Map[String, Object], String) => ?, A]) {
//
//   def read(c: Map[String, Object], key: String): A =
//     run.k(run.fi(c, key))
//
//   def map[B](f: A => B): ConfigReader[B] =
//     new ConfigReader(run map f)
//
// }
//
// object ConfigReader extends ConfigReaderLow {
//
//   def apply[A](implicit ev: ConfigReader[A]): ConfigReader[A] = ev
//
//   def lift[A](f: (Map[String, Object], String) => A): ConfigReader[A] =
//     new ConfigReader(Coyoneda.lift[(Map[String, Object], String) => ?, A](f))
//
//   def cast[A](implicit ev: ClassTag[A]): ConfigReader[A] =
//     lift((c, s) => ev.runtimeClass.cast(c(s)).asInstanceOf[A])
//
//   implicit val StringConfigReader: ConfigReader[String] =
//     cast[Object].map(_.toString)
//
//   // not implicit
//   val OffsetAngleConfigReader: ConfigReader[Angle] =
//     StringConfigReader.map(_.toDouble).map(Angle.fromArcsecs)
//
//   implicit val OffsetPConfigReader: ConfigReader[OffsetP] =
//     OffsetAngleConfigReader.map(OffsetP(_))
//
//   implicit val OffsetQConfigReader: ConfigReader[OffsetQ] =
//     OffsetAngleConfigReader.map(OffsetQ(_))
//
//   implicit val GCalLampConfigReader: ConfigReader[GCalLamp] =
//     cast[java.util.Set[Object]].map(_.iterator.next.toString).map(s => GCalLamp.all.find(_.tccValue === s).getOrElse(sys.error("GCalLampConfigReader: unexpected tccValue: " + s)))
//
//   // implicit def EnumeratedConfigReader[A <: { def tccValue: String }](implicit e: Enumerated[A]) =
//   //   StringConfigReader.map(s => e.all.find(_.tccValue === s).getOrElse("Invalid tccValue: " + s))
//
//   implicit val InstrumentConfigReader: ConfigReader[Instrument] =
//     StringConfigReader.map(s => Instrument.all.find(_.tccValue === s).getOrElse(sys.error("InstrumentConfigReader: unexpected tccValue: " + s)))
//
//   implicit val GCalShutterConfigReader: ConfigReader[GCalShutter] =
//     StringConfigReader.map(s => GCalShutter.all.find(_.tccValue === s).getOrElse(sys.error("GCalShutterConfigReader: unexpected tccValue: " + s)))
//
// }
//
// trait ConfigReaderLow { this: ConfigReader.type =>
//
//   implicit def OptionConfigReader[A](implicit ev: ConfigReader[A]): ConfigReader[Option[A]] =
//     lift((c, s) => c.get(s).as(ev.read(c, s)))
//
// }
//
// object ConfigSyntax {
//
//   implicit class ConfigOps(c: Map[String, Object]) {
//     def read[A](key: String)(implicit ev: ConfigReader[A]): A =
//       ev.read(c, key)
//   }
//
// }
