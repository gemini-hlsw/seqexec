// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gsp.math.optics.Format

/**
 * Given an Format[A, B] and a Composite[A] for the external type, we can create a Composite[B] that
 * will raise an exception if validation fails on read.
 */
class FormatOps[A, B](f: Format[A, B]) {

  def toRead(implicit mb: Read[A]): Read[B] =
    mb.map(f.unsafeGet(_))

  def toOptionRead(implicit mb: Read[Option[A]]): Read[Option[B]] =
    mb.map(_.map(a => f.unsafeGet(a)))

 def toWrite(implicit mb: Write[A]): Write[B] =
    mb.contramap(f.reverseGet)

  def toOptionWrite(implicit mb: Write[Option[A]]): Write[Option[B]] =
    mb.contramap(_.map(f.reverseGet))

}

trait FormatComposite {
  implicit def toFormatOps[A, B](f: Format[A, B]): FormatOps[A, B] =
    new FormatOps(f)
}

object FormatComposite extends FormatComposite
