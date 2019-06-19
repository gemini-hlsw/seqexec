// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.json.instances

import gem.json.syntax.splitepi._
import gsp.math.Offset
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto._

trait OffsetJson {

  implicit val OffsetPEncoder: Encoder[Offset.P] = Offset.P.signedArcseconds.reverse.toEncoder
  implicit val OffsetPDecoder: Decoder[Offset.P] = Offset.P.signedArcseconds.reverse.toDecoder

  implicit val OffsetQEncoder: Encoder[Offset.Q] = Offset.Q.signedArcseconds.reverse.toEncoder
  implicit val OffsetQDecoder: Decoder[Offset.Q] = Offset.Q.signedArcseconds.reverse.toDecoder

  implicit val OffsetEncoder: Encoder[Offset] = deriveEncoder
  implicit val OffsetDecoder: Decoder[Offset] = deriveDecoder

}
object offset extends OffsetJson