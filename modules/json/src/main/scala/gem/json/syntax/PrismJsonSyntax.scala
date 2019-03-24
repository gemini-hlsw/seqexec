// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.syntax

import io.circe._
import monocle.Prism

trait PrismJsonSyntax {
  implicit class JsonPrismOps[A: Encoder: Decoder, B](p: Prism[A, B]) {
    def toEncoder: Encoder[B] = Encoder[A].contramap(p.reverseGet)
    def toDecoder: Decoder[B] = Decoder[A].emap { a =>
      p.getOption(a).toRight(s"getOption failed: $a")
    }
  }
}
object prism extends PrismJsonSyntax
