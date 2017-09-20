// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

import doobie._
import java.time.{ Duration, ZoneId }
import cats.data.NonEmptyList, cats.implicits._
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.record._
import shapeless.record._

/** Scala source for an enumeated type, with a suggested filename. */
final case class EnumDef(fileName: String, text: String)

object EnumDef {
  import Angle._

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference", "org.wartremover.warts.ExplicitImplicitTypes"))
  protected[sql] object ToDeclaration extends Poly1 {
    // scalastyle:off method.type
    implicit def caseString  [S <: Symbol] = at[(S, String)  ] { case (s, _) => "  val " + s.name + ": String" }
    implicit def caseInt     [S <: Symbol] = at[(S, Int)     ] { case (s, _) => "  val " + s.name + ": Int" }
    implicit def caseBoolean [S <: Symbol] = at[(S, Boolean) ] { case (s, _) => "  val " + s.name + ": Boolean" }
    implicit def caseDouble  [S <: Symbol] = at[(S, Double)  ] { case (s, _) => "  val " + s.name + ": Double" }
    implicit def caseDuration[S <: Symbol] = at[(S, Duration)] { case (s, _) => "  val " + s.name + ": java.time.Duration" }
    implicit def caseArcseconds [S <: Symbol] = at[(S, Arcseconds) ] { case (s, _) => "  val " + s.name + ": gem.math.Angle"}
    implicit def caseDegrees [S <: Symbol] = at[(S, Degrees) ] { case (s, _) => "  val " + s.name + ": gem.math.Angle"}
    implicit def caseZoneId  [S <: Symbol] = at[(S, ZoneId)  ] { case (s, _) => "  val " + s.name + ": java.time.ZoneId"}

    implicit def caseWavelengthNm[S <: Symbol] = at[(S, Wavelength.Nm ) ] { case (s, _) => "  val " + s.name + ": gem.math.Wavelength"}
    implicit def caseWavelengthUm[S <: Symbol] = at[(S, Wavelength.Um ) ] { case (s, _) => "  val " + s.name + ": gem.math.Wavelength"}

    implicit def caseOptionArcseconds [S <: Symbol] = at[(S, Option[Arcseconds]) ] { case (s, _) => "  val " + s.name + ": Option[gem.math.Angle]" }
    implicit def caseOptionDegrees [S <: Symbol] = at[(S, Option[Degrees]) ] { case (s, _) => "  val " + s.name + ": Option[gem.math.Angle]" }
    implicit def caseOptionDouble[S <: Symbol] = at[(S, Option[Double]) ] { case (s, _) => "  val " + s.name + ": Option[Double]" }

    implicit def caseOptionWavelengthNm[S <: Symbol] = at[(S, Option[Wavelength.Nm])] { case (s, _) => s"  val ${s.name}: Option[gem.math.Wavelength]" }
    implicit def caseOptionWavelengthUm[S <: Symbol] = at[(S, Option[Wavelength.Um])] { case (s, _) => s"  val ${s.name}: Option[gem.math.Wavelength]" }
    // scalastyle:on method.type
  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference", "org.wartremover.warts.ExplicitImplicitTypes"))
  protected[sql] object ToLiteral extends Poly1 {
    implicit val caseString       = at[String  ](a => "\"" + a + "\"")
    implicit val caseInt          = at[Int     ](a => a.toString)
    implicit val caseBoolean      = at[Boolean ](a => a.toString)
    implicit val caseDouble       = at[Double  ](a => a.toString)
    implicit val caseDuration     = at[Duration](a => s"java.time.Duration.ofMillis(${a.toMillis})")
    implicit val caseArcseconds   = at[Arcseconds](a => s"gem.math.Angle.fromDoubleArcseconds(${a.toArcsecs})")
    implicit val caseDegrees      = at[Degrees](a => s"gem.math.Angle.fromDoubleDegrees(${a.toDegrees})")
    implicit val caseZoneId       = at[ZoneId  ](a => s"""java.time.ZoneId.of("${a.toString}")""")

    implicit val caseWavelengthNm = at[Wavelength.Nm ](a => s"gem.math.Wavelength.unsafeFromAngstroms(${a.toAngstrom})")
    implicit val caseWavelengthUm = at[Wavelength.Um ](a => s"gem.math.Wavelength.unsafeFromAngstroms(${a.toAngstrom})")

    implicit val caseOptionArcseconds = at[Option[Arcseconds]](a => a.fold("Option.empty[gem.math.Angle]")(aʹ => s"Some(gem.math.Angle.fromDoubleArcseconds(${aʹ.toArcsecs}))"))
    implicit val caseOptionDegrees = at[Option[Degrees]](a => a.fold("Option.empty[gem.math.Angle]")(aʹ => s"Some(gem.math.Angle.fromDoubleDegrees(${aʹ.toDegrees}))"))
    implicit val caseOptionDouble = at[Option[Double]](a => a.fold("None")(aʹ => s"Some($aʹ)"))

    implicit val caseOptionWavelengthNm = at[Option[Wavelength.Nm]](a => a.fold("Option.empty[gem.math.Wavelength]")(aʹ => s"Some(gem.math.Wavelength.unsafeFromAngstroms(${aʹ.toAngstrom}))"))
    implicit val caseOptionWavelengthUm = at[Option[Wavelength.Um]](a => a.fold("Option.empty[gem.math.Wavelength]")(aʹ => s"Some(gem.math.Wavelength.unsafeFromAngstroms(${aʹ.toAngstrom}))"))
  }

  private def constructor[H <: HList, O <: HList, L](name: String, id: String, h: H)(
    implicit ma: Mapper.Aux[ToLiteral.type, H, O],
             ta: ToTraversable.Aux[O, List, L]
  ): String =
    h.map(ToLiteral).toList.mkString(s"  /** @group Constructors */ case object $id extends $name(", ", ", ")")

  private def declaration[H <: HList, O <: HList, L](name: String, h: H)(
    implicit ma: Mapper.Aux[ToDeclaration.type, H, O],
             ta: ToTraversable.Aux[O, List, L]
  ): String =
    h.map(ToDeclaration).toList.mkString(s"sealed abstract class $name(\n", ",\n", "\n) extends Product with Serializable")

  // scalastyle:off method.length
  def fromRecords[R <: HList, F <: HList, D <: HList, V <: HList, Lub1, Lub2, L <: HList](name: String, desc: String, records: NonEmptyList[(String, R)])(
    implicit  f: Fields.Aux[R, F],
              d: Mapper.Aux[ToDeclaration.type, F, D],
             t1: ToTraversable.Aux[D, List, Lub1],
              v: Values.Aux[R, V],
             ma: Mapper.Aux[ToLiteral.type, V, L],
             t2: ToTraversable.Aux[L, List, Lub2]
  ): EnumDef =
    EnumDef(s"$name.scala", s"""
      |package gem
      |package enum
      |
      |import cats.syntax.eq._
      |import cats.instances.string._
      |import gem.util.Enumerated
      |
      |/**
      | * Enumerated type for $desc.
      | * @group Enumerations (Generated)
      | */
      |${declaration(name, records.head._2.fields)}
      |
      |object $name {
      |
      |${records.map { case (id, r) => constructor(name, id, r.values) }.intercalate("\n") }
      |
      |  /** All members of $name, in canonical order. */
      |  val all: List[$name] =
      |    List(${records.map(_._1).intercalate(", ")})
      |
      |  /** Select the member of $name with the given tag, if any. */
      |  def fromTag(s: String): Option[$name] =
      |    all.find(_.tag === s)
      |
      |  /** Select the member of $name with the given tag, throwing if absent. */
      |  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      |  def unsafeFromTag(s: String): $name =
      |    fromTag(s).getOrElse(throw new NoSuchElementException(s))
      |
      |  /** @group Typeclass Instances */
      |  implicit val ${name}Enumerated: Enumerated[$name] =
      |    new Enumerated[$name] {
      |      def all = $name.all
      |      def tag(a: $name) = a.tag
      |    }
      |
      |}
      |""".stripMargin.trim
    )
  // scalastyle:on method.length

  def fromQuery[R <: HList, F <: HList, D <: HList, V <: HList, Lub1, Lub2, L <: HList](name: String, desc: String)(records: Query0[(String, R)])(
    implicit  f: Fields.Aux[R, F],
              d: Mapper.Aux[ToDeclaration.type, F, D],
             t1: ToTraversable.Aux[D, List, Lub1],
              v: Values.Aux[R, V],
             ma: Mapper.Aux[ToLiteral.type, V, L],
             t2: ToTraversable.Aux[L, List, Lub2]
  ): ConnectionIO[EnumDef] =
    records.nel.map(fromRecords(name, desc, _))

}
