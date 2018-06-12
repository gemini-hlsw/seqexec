// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.syntax

import gem.optics._
import gem.syntax.prism._
import io.circe._
import monocle.Prism

trait PrismSyntax {
  implicit class JsonPrismOps[A: Encoder: Decoder, B](p: Prism[A, B]) {
    def toEncoder: Encoder[B] = Encoder[A].contramap(p.reverseGet)
    def toDecoder: Decoder[B] = Decoder[A].map(p.unsafeGet(_))
  }
}
object prism extends PrismSyntax

trait SplitMonoSyntax {
  implicit class JsonSplitMonoOps[A: Encoder: Decoder, B](p: SplitMono[A, B]) {
    def toEncoder: Encoder[B] = Encoder[A].contramap(p.reverseGet)
    def toDecoder: Decoder[B] = Decoder[A].map(p.get(_))
  }
}
object splitmono extends SplitMonoSyntax

trait SplitEpiSyntax {
  implicit class JsonSplitEpiOps[A: Encoder: Decoder, B](p: SplitEpi[A, B]) {
    def toEncoder: Encoder[B] = Encoder[A].contramap(p.reverseGet)
    def toDecoder: Decoder[B] = Decoder[A].map(p.get(_))
  }
}
object splitepi extends SplitEpiSyntax

trait FormatSyntax {
  implicit class JsonFormatOps[A: Encoder: Decoder, B](p: Format[A, B]) {
    def toEncoder: Encoder[B] = Encoder[A].contramap(p.reverseGet)
    def toDecoder: Decoder[B] = Decoder[A].map(p.unsafeGet(_))
  }
}
object format extends FormatSyntax

trait OpticsSyntax
  extends PrismSyntax
     with SplitMonoSyntax
     with SplitEpiSyntax
     with FormatSyntax

object optics extends OpticsSyntax
