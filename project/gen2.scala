import doobie.imports._

import java.io.File

import scalaz._, Scalaz._
import scalaz.effect._
import scala.reflect.runtime.universe.TypeTag

import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.syntax._
import shapeless.syntax.singleton._
import shapeless.record._

object gen2 {
  
  val xa = DriverManagerTransactor[IO](
    "org.postgresql.Driver", 
    "jdbc:postgresql:gem", 
    "postgres", 
    ""
  )

  object ToDeclaration extends Poly1 {
    implicit def caseString [S <: Symbol] = at[(S, String) ] { case (s, _) => "  val " + s.name + ": String" }
    implicit def caseInt    [S <: Symbol] = at[(S, Int)    ] { case (s, _) => "  val " + s.name + ": Int" }
    implicit def caseBoolean[S <: Symbol] = at[(S, Boolean)] { case (s, _) => "  val " + s.name + ": Boolean" }
    implicit def caseDouble [S <: Symbol] = at[(S, Double) ] { case (s, _) => "  val " + s.name + ": Double" }

    implicit def caseOptionDouble [S <: Symbol] = at[(S, Option[Double]) ] { case (s, _) => "  val " + s.name + ": Option[Double]" }
  }

  object ToLiteral extends Poly1 {
    implicit val caseString  = at[String ](a => "\"" + a + "\"")
    implicit val caseInt     = at[Int    ](a => a.toString)
    implicit val caseBoolean = at[Boolean](a => a.toString)
    implicit val caseDouble = at[Double](a => a.toString)
    implicit val caseOptionDouble  = at[Option[Double]](a => a.toString)
  }

  def cons[H <: HList, O <: HList, L](name: String, id: String, h: H)(
    implicit ma: Mapper.Aux[ToLiteral.type, H, O],
             ta: ToTraversable.Aux[O, List, L]
  ): String =
    h.map(ToLiteral).toList.mkString(s"  case object $id extends $name(", ", ", ")")


  def decl[H <: HList, O <: HList, L](name: String, h: H)(
    implicit ma: Mapper.Aux[ToDeclaration.type, H, O],
             ta: ToTraversable.Aux[O, List, L]
  ): String =
    h.map(ToDeclaration).toList.mkString(s"sealed abstract class $name(\n", ",\n", "\n)")

  def enum[R <: HList, F <: HList, D <: HList, V <: HList, Lub1, Lub2, L <: HList](name: String)(records: List[(String, R)])(
    implicit  f: Fields.Aux[R, F],
              d: Mapper.Aux[ToDeclaration.type, F, D],
             t1: ToTraversable.Aux[D, List, Lub1],
              v: Values.Aux[R, V],
             ma: Mapper.Aux[ToLiteral.type, V, L],
             t2: ToTraversable.Aux[L, List, Lub2]
  ): (String, String) = 
    (s"$name.scala", s"""
      |package gem
      |package enum
      |
      |import scalaz.syntax.equal._
      |import scalaz.std.string._
      |
      |${decl(name, records.head._2.fields)}
      |
      |object $name {
      |
      |${records.map { case (id, r) => cons(name, id, r.values) }.mkString("\n") }
      |
      |  val all: List[$name] =
      |    List(${records.map(_._1).mkString(", ")})
      |
      |  def fromTag(s: String): Option[$name] =
      |    all.find(_.tag === s)
      |
      |  def unsafeFromTag(s: String): $name =
      |    fromTag(s).getOrElse(throw new NoSuchElementException(s))
      |
      |  implicit val ${name}Enumerated: Enumerated[$name] =
      |    new Enumerated[$name] {
      |      def all = $name.all
      |      def tag(a: $name) = a.tag
      |    }
      |
      |}
      |""".stripMargin.trim
    )

  // Still slightly boilerplatey but not too bad

  val enums: List[(String, String)] =
    List(

      enum("F2Disperser") {
        type F2DisperserRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'tccValue -> String, 'wavelength -> Option[Double]`.T
        val io = sql"select id, id tag, short_name, long_name, tcc_value, wavelength from e_f2_disperser".query[(String, F2DisperserRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("F2Filter") {
        type F2FilterRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'tccValue -> String, 'wavelength -> Double, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, tcc_value, wavelength, obsolete from e_f2_filter".query[(String, F2FilterRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("F2FpUnit") {
        type F2FpUnitRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'tccValue -> String, 'slitWidth -> Int, 'decker -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, tcc_value, slit_width, decker, obsolete from e_f2_fpunit".query[(String, F2FpUnitRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("F2LyotWheel") {
        type F2LyotWheelRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'tccValue -> String, 'plateScale -> Double, 'pixelScale -> Double, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, tcc_value, plate_scale, pixel_scale, obsolete from e_f2_lyot_wheel".query[(String, F2LyotWheelRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GCalFilter") {
        type GcalFilterRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'tccValue -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, tcc_value, obsolete from e_gcal_filter".query[(String, GcalFilterRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GCalLamp") {
        type GcalLampRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'tccValue -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, tcc_value, obsolete from e_gcal_lamp".query[(String, GcalLampRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("GCalShutter") {
        type GcalShutterRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'tccValue -> String`.T
        val io = sql"""
          SELECT enumlabel x, enumlabel a, enumlabel b, enumlabel c, enumlabel d
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'gcal_shutter'
         """.query[(String, GcalShutterRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("StepType") {
        type StepTypeRec = Record.`'tag -> String`.T
        val io = sql"""
          SELECT enumlabel x, enumlabel y
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'step_type'
         """.query[(String, StepTypeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("Instrument") {
        type InstrumentRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'tccValue -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, tcc_value, obsolete from e_instrument".query[(String, InstrumentRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("ProgramType") {
        type ProgramTypeRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String, 'obsolete -> Boolean`.T
        val io = sql"select id, id tag, short_name, long_name, obsolete from e_program_type".query[(String, ProgramTypeRec)].list
        io.transact(xa).unsafePerformIO
      },

      enum("Site") {
        type SiteRec = Record.`'tag -> String, 'shortName -> String, 'longName -> String`.T
        val io = sql"select id, id tag, short_name, long_name from e_site".query[(String, SiteRec)].list
        io.transact(xa).unsafePerformIO
      }

    )

  def apply(dir: File): IO[Seq[File]] =
    for {
      fs <- enums.traverse { case (f, c) => IO { 
              val file = new File(dir, f)
              sbt.IO.write(file, c)
              file
            }}
    } yield fs

}


