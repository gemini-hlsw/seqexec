// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import cats.Order
import scala.collection.immutable.SortedMap
import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }

trait SortedMapJson {

  implicit def SortedMapEncoder[K: KeyEncoder, V: Encoder]: Encoder[SortedMap[K, V]] =
    Encoder[Map[K, V]].contramap(identity)

  implicit def SortedMapDecoder[K: KeyDecoder: Order, V: Decoder]: Decoder[SortedMap[K, V]] = {
    implicit val ord: Ordering[K] = Order[K].toOrdering
    Decoder[Map[K, V]].map(m => SortedMap(m.toSeq: _*))
  }

}
object sortedmap extends SortedMapJson