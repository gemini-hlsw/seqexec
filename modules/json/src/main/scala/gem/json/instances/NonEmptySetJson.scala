// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.Order
import cats.data.{ NonEmptyList, NonEmptySet }
import io.circe.{ Decoder, Encoder }

trait NonEmptySetJson {

  implicit def nonEmptySetEncoder[A: Encoder]: Encoder[NonEmptySet[A]] =
    Encoder[NonEmptyList[A]].contramap(s => NonEmptyList.of(s.head, s.tail.toList: _*))

  implicit def nonEmptySetDecoder[A: Order: Decoder]: Decoder[NonEmptySet[A]] =
    Decoder[NonEmptyList[A]].map(l => NonEmptySet.of(l.head, l.tail: _*))

}
object nonemptyset extends NonEmptySetJson