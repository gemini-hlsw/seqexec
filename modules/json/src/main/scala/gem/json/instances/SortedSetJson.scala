// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.Order
import scala.collection.immutable.SortedSet
import io.circe.{ Decoder, Encoder }

trait SortedSetJson {

  implicit def SortedSetEncoder[A: Encoder]: Encoder[SortedSet[A]] =
    Encoder[Set[A]].contramap(identity)

  implicit def SortedSetDecoder[A: Order: Decoder]: Decoder[SortedSet[A]] = {
    implicit val ord: Ordering[A] = Order[A].toOrdering
    Decoder[Set[A]].map(_.to[SortedSet])
  }

}
object sortedset extends SortedSetJson