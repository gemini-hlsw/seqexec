// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import cats._, cats.data._, cats.implicits._

package object pio {
  type PioOptional[A] = OptionT[PioError \/ ?, A]

  object PioOptional {
    def apply[A](a: PioError \/ Option[A]): PioOptional[A] =
      OptionT[PioError \/ ?, A](a)

    def fromOption[A](oa: Option[A]): PioOptional[A] =
      OptionT[PioError \/ ?, A](oa.right)

    def none[A]: PioOptional[A] =
      OptionT.none[PioError \/ ?, A]

    def some[A](a: A): PioOptional[A] =
      OptionT.some[PioError \/ ?, A](a)
  }
}
