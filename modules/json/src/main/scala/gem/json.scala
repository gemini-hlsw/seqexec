package gem

import edu.gemini.spModel.core._
import gem.enum.Instrument
import gem.config._
import java.time.Duration
import argonaut._, Argonaut._, ArgonautShapeless._

// These json codecs are provided for primitive types that have no natural mapping that would
// otherwise be inferred via argonaut-shapeless. For now we'll treat the JSON format as an
// internal concern rather than a public or long-term storage format. If we decide that we *do*
// want to use JSON externally then auto-derivation becomes dangerous because renaming fields
// will cause the serial format to change.
package object json {

  // Angle mapping to signed arcseconds. NOT implicit.
  val AngleMetaAsSignedArcseconds: CodecJson[Angle] =
    CodecJson.derived[Double].xmap(Angle.fromArcsecs)(_.toSignedDegrees * 3600)

  implicit val durationCodec: CodecJson[Duration] =
    CodecJson(
      d => Json("seconds" := d.getSeconds, "nanoseconds" := d.getNano),
      c => for {
             ss <- (c --\ "seconds").as[Long]
             ns <- (c --\ "nanoseconds").as[Long]
           } yield Duration.ofSeconds(ss, ns)
    )

  implicit def enumeratedCodec[A](implicit ev: Enumerated[A]): CodecJson[A] =
    CodecJson.derived[String].xmap(ev.unsafeFromTag)(ev.tag)

  implicit val programIdCodec: CodecJson[Program.Id] =
    CodecJson.derived[String].xmap(Program.Id.parse)(_.toString)

  implicit val observationIdCodec: CodecJson[Observation.Id] =
    casecodec2(Observation.Id.apply, Observation.Id.unapply)("program-id", "index")

  // OffsetP maps to a signed angle in arcseconds
  implicit val OffsetPMeta: CodecJson[OffsetP] =
    AngleMetaAsSignedArcseconds.xmap(OffsetP(_))(_.toAngle)

  // OffsetQ maps to a signed angle in arcseconds
  implicit val OffsetQMeta: CodecJson[OffsetQ] =
    AngleMetaAsSignedArcseconds.xmap(OffsetQ(_))(_.toAngle)

}
