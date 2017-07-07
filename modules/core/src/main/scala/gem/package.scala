// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import scalaz.{ Order, Ordering }
import scalaz.Ordering._
import java.time.Instant

package object gem {

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val InstantOrd: Order[Instant] =
    new Order[Instant] {
      def order(a: Instant, b: Instant): Ordering =
             if (a == b)       EQ
        else if (a isBefore b) LT
        else                   GT
    }

}
