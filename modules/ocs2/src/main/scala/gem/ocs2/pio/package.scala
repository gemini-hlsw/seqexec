// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import cats.data._, cats.implicits._

package object pio {
  type PioOptional[A] = OptionT[Either[PioError, ?], A]

  object PioOptional {
    def apply[A](a: Either[PioError, Option[A]]): PioOptional[A] =
      OptionT[Either[PioError, ?], A](a)

    def fromOption[A](oa: Option[A]): PioOptional[A] =
      OptionT[Either[PioError, ?], A](oa.asRight)

    def none[A]: PioOptional[A] =
      OptionT.none[Either[PioError, ?], A]

    def some[A](a: A): PioOptional[A] =
      OptionT.some[Either[PioError, ?]](a)
  }
}
