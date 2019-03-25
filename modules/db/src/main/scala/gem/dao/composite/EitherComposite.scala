// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._

trait EitherComposite {

  /** Two-column mapping for Either. */
  implicit def eitherRead[A: Meta, B: Meta]: Read[Either[A, B]] =
    Read[(Option[A], Option[B])].map {
      case (Some(a), None) => Left(a)
      case (None, Some(b)) => Right(b)
      case (a, b) => sys.error(s"Invariant violated: can't map ($a, $b) to Either!")
    }

  /** Two-column mapping for Either. */
  implicit def eitherWrite[A: Meta, B: Meta]: Write[Either[A, B]] =
    Write[(Option[A], Option[B])].contramap {
      case Left(a) => (Some(a), None)
      case Right(b) => (None, Some(b))
    }

}
object EitherComposite extends EitherComposite
