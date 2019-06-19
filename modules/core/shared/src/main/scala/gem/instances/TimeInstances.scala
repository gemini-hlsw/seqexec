// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.instances

import java.time._
import cats.{ Eq, Order }
import cats.implicits._

/** Instances for java.time data types. */
trait TimeInstances {

  // The java.time types all follow this pattern.
  private def naturalOrder[A](before: (A, A) => Boolean): Order[A] =
    Order.from { (a, b) =>
             if (a == b)        0
        else if (before(a, b)) -1
        else                    1
    }

  // This looks like repetition but it's not. The isBefore methods have the same name but are
  // unrelated by any common supertype.
  implicit val InstantOrder:       Order[Instant]       = naturalOrder(_ isBefore _)
  implicit val YearOrder:          Order[Year]          = naturalOrder(_ isBefore _)
  implicit val LocalTimeOrder:     Order[LocalTime]     = naturalOrder(_ isBefore _)
  implicit val LocalDateOrder:     Order[LocalDate]     = naturalOrder(_ isBefore _)
  implicit val LocalDateTimeOrder: Order[LocalDateTime] = naturalOrder(_ isBefore _)

  implicit val DurationEq: Eq[Duration] =
    Eq.by(d => (d.getSeconds, d.getNano))

}
object time extends TimeInstances
