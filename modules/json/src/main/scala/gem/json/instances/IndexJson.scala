// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.math.Index
import gem.json.syntax.prism._
import gem.syntax.prism._
import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }

trait IndexJson {

  implicit val IndexEncoder: Encoder[Index] = Index.fromShort.toEncoder
  implicit val IndexDecoder: Decoder[Index] = Index.fromShort.toDecoder

  implicit val IndexKeyEncoder: KeyEncoder[Index] = KeyEncoder[Short].contramap(_.toShort)
  implicit val IndexKeyDecoder: KeyDecoder[Index] = KeyDecoder[Short].map(Index.fromShort.unsafeGet)

}
object index extends IndexJson