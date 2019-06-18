// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gsp.math.optics.Format
import scala.reflect.runtime.universe.TypeTag

/**
 * Given an Format[A, B] and a Meta[A] for the external format, we can create a Meta[B] that will
 * raise an exception if validation fails on read.
 */
class FormatOps[A, B](f: Format[A, B]) {
  def asMeta(
    implicit mb: Meta[A],
             ta: TypeTag[B]
  ): Meta[B] =
    mb.timap(f.getOption(_).getOrElse(sys.error("Validation failed.")))(f.reverseGet)
}

trait FormatMeta {
  implicit def toFormatOps[A, B](f: Format[A, B]): FormatOps[A, B] =
    new FormatOps(f)
}

object FormatMeta extends FormatMeta
