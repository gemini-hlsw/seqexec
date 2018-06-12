// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._

trait EitherComposite {

  /** Two-column mapping for Either. */
  implicit def eitherComposite[A: Meta, B: Meta]: Composite[Either[A, B]] =
    Composite[(Option[A], Option[B])].imap {
      case (Some(a), None) => Left(a)
      case (None, Some(b)) => Right(b)
      case (a, b) => sys.error(s"Invariant violated: can't map ($a, $b) to Either!")
    } {
      case Left(a) => (Some(a), None)
      case Right(b) => (None, Some(b))
    }

}
object EitherComposite extends EitherComposite
