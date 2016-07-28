import doobie.imports._

import scalaz._, Scalaz._
import scalaz.effect._
import scala.reflect.runtime.universe.TypeTag

import sbt.{ IO => _, _ }

object gen extends SafeApp {
  
  val xa = DriverManagerTransactor[IO](
    "org.postgresql.Driver", 
    "jdbc:postgresql:gem", 
    "postgres", 
    ""
  )

  sealed trait Gen[A] {
    def cname: String
    def mkClass: String
    def mkCase(a: A): String
    def tag(a: A): String
    def query: Query0[A]
    def code: IO[(String, String)] =
      query.list.transact(xa).map { as =>
       (s"$cname.scala",
        s"""|// GENERATED CODE; DO NOT MODIFY
            |package gem
            |
            |$mkClass
            |  extends Product with Serializable
            |
            |object $cname {
            |
            ${as.map(mkCase).mkString("|  ", "\n|  ", "")}
            |
            |  implicit val ${cname}Enumerated: Enumerated[${cname}] =
            |    new Enumerated[${cname}] {
            |      def tag(a: ${cname}) = a.tag
            |      val all = List(${as.map(x => sanitize(tag(x)).capitalize).mkString(", ")})
            |    }
            |
            |}
            |""".stripMargin)
      }
  }
  object Gen {
    def apply[A](implicit ev: Gen[A]): Gen[A] = ev
  }

  def sanitize(s: String): String =
    s.replace('-', '_').filterNot(_.isWhitespace)

  case class ProgramType(tag: String, name: String)
  object ProgramType {
    implicit val GenProgramType: Gen[ProgramType] =
      new Gen[ProgramType] {
        val cname = "ProgramType"
        def mkClass = "sealed abstract class ProgramType(val tag: String, val name: String)"
        def mkCase(p: ProgramType) = f"""case object ${p.tag}%-3s extends ProgramType("${p.tag}", "${p.name}")"""
        def tag(p: ProgramType) = p.tag
        val query =
         sql"""
              SELECT program_type_id, name
                FROM program_type
            ORDER BY name ASC
          """.query[ProgramType]  
      }
  }

  case class ChargeClass(tag: String, name: String)
  object ChargeClass {
    implicit val GenChargeClass: Gen[ChargeClass] =
      new Gen[ChargeClass] {
        val cname = "ChargeClass"
        def mkClass = "sealed abstract class ChargeClass(val tag: String, val name: String)"
        def mkCase(p: ChargeClass) = f"""case object ${p.tag.capitalize} extends ChargeClass("${p.tag}", "${p.name}")"""
        def tag(p: ChargeClass) = p.tag
        val query =
         sql"""
              SELECT charge_class_id, name
                FROM charge_class
            ORDER BY name ASC
          """.query[ChargeClass]  
      }
  }

  case class ObsClass(tag: String, name: String, cc: String, log: String)
  object ObsClass {
    implicit val GenChargeClass: Gen[ObsClass] =
      new Gen[ObsClass] {
        val cname = "ObsClass"
        def mkClass = "sealed abstract class ObsClass(val tag: String, val name: String, val chargeClass: ChargeClass, val logValue: String)"
        def mkCase(p: ObsClass) = f"""case object ${p.tag.capitalize} extends ObsClass("${p.tag}", "${p.name}", ChargeClass.${p.cc.capitalize}, "${p.log}")"""
        def tag(p: ObsClass) = p.tag
        val query =
         sql"""
              SELECT obs_class_id, name, charge_class_id, log_value
                FROM obs_class
            ORDER BY priority ASC
          """.query[ObsClass]  
      }
  }

  case class Instrument(tag: String, name: String)
  object Instrument {
    implicit val GenChargeClass: Gen[Instrument] =
      new Gen[Instrument] {
        val cname = "Instrument"
        def mkClass = "sealed abstract class Instrument(val tag: String, val name: String)"
        def mkCase(p: Instrument) = f"""case object ${sanitize(p.tag).capitalize} extends Instrument("${p.tag}", "${p.name}")"""
        def tag(p: Instrument) = p.tag
        val query =
         sql"""
              SELECT instrument_id, name
                FROM instrument
            ORDER BY name ASC
          """.query[Instrument]  
      }
  }

  def apply(dir: File): IO[Seq[File]] =
    for {
      pairs  <- List(
                  Gen[ProgramType].code,
                  Gen[ChargeClass].code,
                  Gen[ObsClass].code,
                  Gen[Instrument].code
                ).sequence
      fs     <- pairs.traverse { case (f, c) => 
                  IO { 
                    val file = new File(dir, f)
                    sbt.IO.write(file, c)
                    file
                  }
                }
    } yield fs

}


