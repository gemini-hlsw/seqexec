// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.util.Enumerated
import io.circe.{ Decoder, Encoder }

trait EnumeratedJson {

  implicit def enumeratedEncoder[A](implicit ev: Enumerated[A]): Encoder[A] = Encoder[String].contramap(ev.tag)
  implicit def enumeratedDecoder[A](implicit ev: Enumerated[A]): Decoder[A] = Decoder[String].map(ev.unsafeFromTag)

}
object enumerated extends EnumeratedJson