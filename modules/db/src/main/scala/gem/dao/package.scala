package gem

import doobie.postgres.imports._
import doobie.imports._
import edu.gemini.spModel.core._

import java.sql.Timestamp
import java.time.{Duration, Instant}
import java.util.logging.Level

import scala.reflect.runtime.universe.TypeTag
import scalaz._
import Scalaz._

package object dao extends MoreTupleOps with ToUserProgramRoleOps {

  type MaybeConnectionIO[A] = OptionT[ConnectionIO, A]

  object MaybeConnectionIO {
    def apply[A](coa: ConnectionIO[Option[A]]): MaybeConnectionIO[A] =
      OptionT(coa)

    def fromOption[A](oa: Option[A]): MaybeConnectionIO[A] =
      oa.fold(OptionT.none[ConnectionIO, A]) { a =>
        OptionT.some[ConnectionIO, A](a)
      }

    def none[A]: MaybeConnectionIO[A] =
      OptionT.none[ConnectionIO, A]

    def some[A](a: => A): MaybeConnectionIO[A] =
      OptionT.some[ConnectionIO, A](a)
  }

  implicit class Query0Ops[A](a: Query0[A]) {
    def maybe: MaybeConnectionIO[A] =
      MaybeConnectionIO(a.option)
  }

  type EitherConnectionIO[A, B] = EitherT[ConnectionIO, A, B]

  object EitherConnectionIO {
    def apply[A, B](ceab: ConnectionIO[\/[A, B]]): EitherConnectionIO[A, B] =
      EitherT(ceab)

    def left[A, B](a: ConnectionIO[A]): EitherConnectionIO[A, B] =
      EitherT.left[ConnectionIO, A, B](a)

    def pointLeft[A, B](a: A): EitherConnectionIO[A, B] =
      left(a.point[ConnectionIO])

    def right[A, B](b: ConnectionIO[B]): EitherConnectionIO[A, B] =
      EitherT.right[ConnectionIO, A, B](b)

    def pointRight[A, B](b: B): EitherConnectionIO[A, B] =
      right(b.point[ConnectionIO])

    def fromDisjunction[A, B](eab: A \/ B): EitherConnectionIO[A, B] =
      EitherT.fromDisjunction(eab)
  }

  implicit class ConnectionIOOps[T](c: ConnectionIO[T]) {
    def injectLeft[B]: EitherConnectionIO[T, B] =
      EitherConnectionIO.left[T, B](c)

    def injectRight[A]: EitherConnectionIO[A, T] =
      EitherConnectionIO.right[A, T](c)
  }

  // Angle mapping to signed arcseconds. NOT implicit.
  val AngleMetaAsSignedArcseconds: Meta[Angle] =
    Meta[Double].xmap(Angle.fromArcsecs, _.toSignedDegrees * 3600)

  // OffsetP maps to a signed angle in arcseconds
  implicit val OffsetPMeta: Meta[OffsetP] =
    AngleMetaAsSignedArcseconds.xmap(OffsetP(_), _.toAngle)

  // OffsetQ maps to a signed angle in arcseconds
  implicit val OffsetQMeta: Meta[OffsetQ] =
    AngleMetaAsSignedArcseconds.xmap(OffsetQ(_), _.toAngle)

  // Program.Id as string
  implicit val ProgramIdMeta: Meta[Program.Id] =
    Meta[String].nxmap(Program.Id.parse, _.toString)

  // Observation.Id as string
  implicit val ObservationIdMeta: Meta[Observation.Id] =
    Meta[String].nxmap(Observation.Id.unsafeFromString, _.toString)

  // Dataset.Label as string
  implicit val DatasetLabelMeta: Meta[Dataset.Label] =
    Meta[String].nxmap(Dataset.Label.unsafeFromString, _.toString)

  // Enumerated by tag as string
  implicit def enumeratedMeta[A >: Null : TypeTag](implicit ev: Enumerated[A]): Meta[A] =
    Meta[String].nxmap[A](ev.unsafeFromTag(_), ev.tag(_))

  // Java Log Levels (not nullable)
  implicit def levelMeta: Meta[Level] =
    Meta[String].nxmap(Level.parse, _.getName)

  implicit val InstantMeta: Meta[Instant] =
    Meta[Timestamp].nxmap(_.toInstant, Timestamp.from)

  implicit val LocationMeta: Meta[Location.Middle] =
    Meta[List[Int]].nxmap(Location.unsafeMiddle(_), _.toList)

  implicit val DurationMeta: Meta[Duration] =
    Meta[Long].xmap(Duration.ofMillis, _.toMillis)

  def capply2[A, B, T](f: (A, B) => T)(
    implicit ca: Composite[(Option[A], Option[B])]
  ): Composite[Option[T]] =
    ca.xmap(_.apply2(f), _ => sys.error("decode only"))

  def capply3[A, B, C, T](f: (A, B, C) => T)(
    implicit ca: Composite[(Option[A], Option[B], Option[C])]
  ): Composite[Option[T]] =
    ca.xmap(_.apply3(f), _ => sys.error("decode only"))

  def capply4[A, B, C, D, T](f: (A, B, C, D) => T)(
    implicit ca: Composite[(Option[A], Option[B], Option[C], Option[D])]
  ): Composite[Option[T]] =
    ca.xmap(_.apply4(f), _ => sys.error("decode only"))

}

trait MoreTupleOps {

  import scalaz._, Scalaz._

  implicit class MoreTuple2Ops[F[_], A, B](t: (F[A], F[B]))(implicit ev: Apply[F]) {
    def apply2[T](f: (A, B) => T): F[T] =
      t.fold(ev.lift2(f))
  }

  implicit class MoreTuple3Ops[F[_], A, B, C](t: (F[A], F[B], F[C]))(implicit ev: Apply[F]) {
    def apply3[T](f: (A, B, C) => T): F[T] =
      t.fold(ev.lift3(f))
  }

  implicit class MoreTuple4Ops[F[_], A, B, C, D](t: (F[A], F[B], F[C], F[D]))(implicit ev: Apply[F]) {
    def apply4[T](f: (A, B, C, D) => T): F[T] =
      t.fold(ev.lift4(f))
  }

}
