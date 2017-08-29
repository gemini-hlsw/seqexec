// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.data._, cats.implicits._
import doobie._, doobie.implicits._
import doobie.enum.jdbctype.{ Distinct => JdbcDistinct, Array => _, _ }
import doobie.postgres.implicits._
import gem.math.{ Angle, Offset, Wavelength }
import gem.util.{ Enumerated, Location }
import java.sql.Timestamp
import java.time.{Duration, Instant}
import java.util.logging.Level
import scala.reflect.runtime.universe.TypeTag

package object dao {

  // Uncomment to turn on statement logging
  // implicit val han = LogHandler.jdkLogHandler

  type MaybeConnectionIO[A] = OptionT[ConnectionIO, A]

  object MaybeConnectionIO {
    def apply[A](coa: ConnectionIO[Option[A]]): MaybeConnectionIO[A] =
      OptionT(coa)

    def fromOption[A](oa: Option[A]): MaybeConnectionIO[A] =
      oa.fold(OptionT.none[ConnectionIO, A]) { a =>
        OptionT.some[ConnectionIO](a)
      }

    def none[A]: MaybeConnectionIO[A] =
      OptionT.none[ConnectionIO, A]

    def some[A](a: => A): MaybeConnectionIO[A] =
      OptionT.some[ConnectionIO](a)
  }

  implicit class Query0Ops[A](a: Query0[A]) {
    def maybe: MaybeConnectionIO[A] =
      MaybeConnectionIO(a.option)
  }

  type EitherConnectionIO[A, B] = EitherT[ConnectionIO, A, B]

  object EitherConnectionIO {
    def apply[A, B](ceab: ConnectionIO[Either[A, B]]): EitherConnectionIO[A, B] =
      EitherT(ceab)

    def left[A, B](a: ConnectionIO[A]): EitherConnectionIO[A, B] =
      EitherT.left(a)

    def pointLeft[A, B](a: A): EitherConnectionIO[A, B] =
      left(a.pure[ConnectionIO])

    def right[A, B](b: ConnectionIO[B]): EitherConnectionIO[A, B] =
      EitherT.right(b)

    def pointRight[A, B](b: B): EitherConnectionIO[A, B] =
      right(b.pure[ConnectionIO])

    def fromDisjunction[A, B](eab: Either[A, B]): EitherConnectionIO[A, B] =
      EitherT.fromEither(eab)
  }

  implicit class ConnectionIOOps[T](c: ConnectionIO[T]) {
    def injectLeft[B]: EitherConnectionIO[T, B] =
      EitherConnectionIO.left[T, B](c)

    def injectRight[A]: EitherConnectionIO[A, T] =
      EitherConnectionIO.right[A, T](c)
  }

  // Angle mapping to signed arcseconds via NUMERIC. NOT implicit. We're mapping a type that
  // is six orders of magnitude more precise than the database column, so we will shift
  // the decimal pure back and forth.
  val AngleMetaAsSignedArcseconds: Meta[Angle] =
    Meta[java.math.BigDecimal]
      .xmap[Angle](
        b => Angle.fromMicroarcseconds(b.movePointRight(6).longValue),
        a => new java.math.BigDecimal(a.toSignedMicroarcseconds).movePointLeft(6)
      )

  // OffsetP maps to a signed angle in arcseconds
  implicit val OffsetPMeta: Meta[Offset.P] =
    AngleMetaAsSignedArcseconds.xmap(Offset.P(_), _.toAngle)

  // OffsetQ maps to a signed angle in arcseconds
  implicit val OffsetQMeta: Meta[Offset.Q] =
    AngleMetaAsSignedArcseconds.xmap(Offset.Q(_), _.toAngle)

  // Wavelength maps to an integer in angstroms
  implicit val WavelengthMeta: Meta[Wavelength] =
    Meta[Int].xmap(Wavelength.unsafeFromAngstroms, _.toAngstroms)

  // Program.Id as string
  implicit val ProgramIdMeta: Meta[Program.Id] =
    Meta[String].xmap(Program.Id.unsafeFromString, _.format)

  // Observation.Id as string
  implicit val ObservationIdMeta: Meta[Observation.Id] =
    Meta[String].xmap(Observation.Id.unsafeFromString, _.format)

  // Dataset.Label as string
  implicit val DatasetLabelMeta: Meta[Dataset.Label] =
    Meta[String].xmap(Dataset.Label.unsafeFromString, _.format)

  // Enumerated by tag as DISTINCT (identifier)
  implicit def enumeratedMeta[A >: Null : TypeTag](implicit ev: Enumerated[A]): Meta[A] =
    Distinct.string("identifier").xmap[A](ev.unsafeFromTag(_), ev.tag(_))

  // Java Log Levels (not nullable)
  implicit def levelMeta: Meta[Level] =
    Meta[String].xmap(Level.parse, _.getName)

  implicit val InstantMeta: Meta[Instant] =
    Meta[Timestamp].xmap(_.toInstant, Timestamp.from)

  implicit val LocationMeta: Meta[Location.Middle] =
    Meta[List[Int]].xmap(Location.unsafeMiddleFromFoldable(_), _.toList)

  implicit val DurationMeta: Meta[Duration] =
    Distinct.long("milliseconds").xmap(Duration.ofMillis, _.toMillis)

  /**
   * Constructor for a Meta instances with an underlying types that are reported by JDBC as
   * type Distinct, as happens when a column has a check constraint. By using a data type with
   * a Distinct Meta instance we can satisfy the query checker.
   */
  object Distinct {

    def integer(name: String): Meta[Int] =
      Meta.advanced(
        NonEmptyList.of(JdbcDistinct, Integer),
        NonEmptyList.of(name),
        _ getInt _,
        _.setInt(_, _),
        _.updateInt(_, _)
      )

    def long(name: String): Meta[Long] =
      Meta.advanced(
        NonEmptyList.of(JdbcDistinct, BigInt),
        NonEmptyList.of(name),
        _ getLong _,
        _.setLong(_, _),
        _.updateLong(_, _)
      )

    def short(name: String): Meta[Short] =
      Meta.advanced(
        NonEmptyList.of(JdbcDistinct, SmallInt),
        NonEmptyList.of(name),
        _ getShort _,
        _.setShort(_, _),
        _.updateShort(_, _)
      )

    def string(name: String): Meta[String] =
      Meta.advanced(
        NonEmptyList.of(JdbcDistinct, VarChar),
        NonEmptyList.of(name),
        _ getString _,
        _.setString(_, _),
        _.updateString(_, _)
      )

  }

  def capply2[A, B, T](f: (A, B) => T)(
    implicit ca: Composite[(Option[A], Option[B])]
  ): Composite[Option[T]] =
    ca.imap(_.mapN(f))(_ => sys.error("decode only"))

  def capply3[A, B, C, T](f: (A, B, C) => T)(
    implicit ca: Composite[(Option[A], Option[B], Option[C])]
  ): Composite[Option[T]] =
    ca.imap(_.mapN(f))(_ => sys.error("decode only"))

  def capply4[A, B, C, D, T](f: (A, B, C, D) => T)(
    implicit ca: Composite[(Option[A], Option[B], Option[C], Option[D])]
  ): Composite[Option[T]] =
    ca.imap(_.mapN(f))(_ => sys.error("decode only"))

}
