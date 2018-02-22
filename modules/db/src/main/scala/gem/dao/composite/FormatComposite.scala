// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gem.optics.Format

/**
 * Given an Format[A, B] and a Composite[A] for the external type, we can create a Composite[B] that
 * will raise an exception if validation fails on read.
 */
class FormatOps[A, B](f: Format[A, B]) {

  def toComposite(implicit mb: Composite[A]): Composite[B] =
    mb.imap(f.unsafeGet(_))(f.reverseGet)

  def toOptionComposite(implicit mb: Composite[Option[A]]): Composite[Option[B]] =
    mb.imap(_.map(a => f.unsafeGet(a)))(_.map(f.reverseGet))

}

trait FormatComposite {
  implicit def toFormatOps[A, B](f: Format[A, B]): FormatOps[A, B] =
    new FormatOps(f)
}

object FormatComposite extends FormatComposite
