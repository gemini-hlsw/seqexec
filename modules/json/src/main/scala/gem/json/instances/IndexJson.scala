// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.json.syntax.prism._
import gsp.math.Index
import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }

trait IndexJson {

  implicit val IndexEncoder: Encoder[Index] = Index.fromShort.toEncoder
  implicit val IndexDecoder: Decoder[Index] = Index.fromShort.toDecoder

  implicit val IndexKeyEncoder: KeyEncoder[Index] = KeyEncoder[Short].contramap(_.toShort)
  implicit val IndexKeyDecoder: KeyDecoder[Index] =
    KeyDecoder.instance { s =>
      KeyDecoder[Short].apply(s).flatMap(Index.fromShort.getOption)
    }

}
object index extends IndexJson