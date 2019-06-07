// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gsp.math.syntax.prism._
import monocle.Prism
import scala.reflect.runtime.universe.TypeTag

/**
 * Given an Prism[A, B] and a Meta[A] for the external Prism, we can create a Meta[B] that will
 * raise an exception if validation fails on read.
 */
class PrismOps[A, B](f: Prism[A, B]) {
  def asMeta(
    implicit mb: Meta[A],
             ta: TypeTag[B]
  ): Meta[B] =
    mb.timap(f.unsafeGet(_))(f.reverseGet)
}

trait PrismMeta {
  implicit def toPrismOps[A, B](f: Prism[A, B]): PrismOps[A, B] =
    new PrismOps(f)
}

object PrismMeta extends PrismMeta
