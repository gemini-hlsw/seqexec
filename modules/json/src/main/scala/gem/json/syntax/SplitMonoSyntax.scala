// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.syntax

import gem.optics._
import io.circe._

trait SplitMonoJsonSyntax {
  implicit class JsonSplitMonoOps[A: Encoder: Decoder, B](p: SplitMono[A, B]) {
    def toEncoder: Encoder[B] = Encoder[A].contramap(p.reverseGet)
    def toDecoder: Decoder[B] = Decoder[A].map(p.get(_))
  }
}
object splitmono extends SplitMonoJsonSyntax
