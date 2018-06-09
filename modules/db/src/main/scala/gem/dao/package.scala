// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.data._, cats.implicits._
import doobie._, doobie.implicits._

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

  /** Flattened `a` as a VALUES argument (...). */
  def values[A](a: A)(implicit ev: Composite[A]): Fragment =
    Fragment(List.fill(ev.length)("?").mkString("(", ", ", ")"), a)

}
