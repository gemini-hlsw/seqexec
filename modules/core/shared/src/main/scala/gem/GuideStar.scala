// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats._
import cats.implicits._

object GuideStar {

  final case class Id(toInt: Int) extends AnyVal

  object Id {
    implicit val IdOrder: Order[Id] =
      Order.by(_.toInt)
  }

}
