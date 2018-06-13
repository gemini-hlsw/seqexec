// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.syntax

import gem.optics.Format
import monocle.Prism

@SuppressWarnings(Array("org.wartremover.warts.Null"))
final class PrismOps[A, B](val self: Prism[A, B]) extends AnyVal {

  /** Weaken to Format. */
  def asFormat: Format[A, B] =
    Format.fromPrism(self)

  /** Like getOption, but throws IllegalArgumentException on failure. */
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def unsafeGet(a: A): B =
    asFormat.unsafeGet(a)

}

trait ToPrismOps {
  implicit def ToPrismOps[A, B](p: Prism[A, B]): PrismOps[A, B] =
    new PrismOps(p)
}

object prism extends ToPrismOps
