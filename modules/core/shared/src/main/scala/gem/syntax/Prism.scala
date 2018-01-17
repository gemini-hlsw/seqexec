// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.syntax

import gem.util.Format
import monocle.Prism
import scala.reflect.runtime.universe.TypeTag

@SuppressWarnings(Array("org.wartremover.warts.Null"))
final class PrismOps[A, B](val self: Prism[A, B]) extends AnyVal {

  /** Weaken to Format. */
  def asFormat: Format[A, B] =
    Format.fromPrism(self)

  /** Like getOption, but throws IllegalArgumentException on failure. */
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def unsafeGet(a: A)(implicit ev: TypeTag[B] = null): B =
    asFormat.unsafeGet(a)

}

trait ToPrismOps {
  implicit def ToPrismOps[A, B](p: Prism[A, B]): PrismOps[A, B] =
    new PrismOps(p)
}

object prism extends ToPrismOps
