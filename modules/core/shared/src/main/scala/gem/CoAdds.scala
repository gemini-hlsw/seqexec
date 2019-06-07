// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Order, Show }
import cats.instances.short._
import gsp.math.syntax.prism._
import monocle.Prism

sealed abstract case class CoAdds private (toShort: Short) {
  // Sanity check … should be correct via the companion constructor.
  assert(toShort > 0, s"Invariant violated. $toShort is 1 or greater.")
}

object CoAdds {

  final val One: CoAdds = fromShort.unsafeGet(1)

  /** @group Typeclass Instances */
  implicit val CoAddsShow: Show[CoAdds] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val CoAddsOrd: Order[CoAdds] =
    Order.by(_.toShort)

  /**
    * Prism from Short into CoAdds and back.
    * @group Optics
    */
  def fromShort: Prism[Short, CoAdds] =
    Prism((n: Short) => Some(n).filter(_ > 0).map(new CoAdds(_) {}))(_.toShort)

}
