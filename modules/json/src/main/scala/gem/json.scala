package gem

import gem.enum.Instrument
import gem.config._
import java.time.Duration
import argonaut._, Argonaut._

object json {

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

  implicit def programCodec[A: CodecJson]: CodecJson[Program[A]] =
    casecodec3(Program.apply[A], Program.unapply[A])("id", "title", "observations")

  implicit val observationIdCodec: CodecJson[Observation.Id] =
    casecodec2(Observation.Id.apply, Observation.Id.unapply)("program-id", "index")

  implicit def observationCodec[A: CodecJson]: CodecJson[Observation[A]] =
    casecodec4(Observation.apply[A], Observation.unapply[A])("id", "title", "instrument", "steps")

   implicit val f2ConfigCodec: CodecJson[F2Config] =
      casecodec6(F2Config.apply, F2Config.unapply)(
        "fpu",
        "mosPreimaging",
        "exposureTime",
        "filter",
        "lyoutWheel",
        "disperser"
      )

  // implicit def instrumentConfigCodec[A](implicit ev: InstrumentConfig[A]): CodecJson[A] =
  //   ev.instrument match {
  //     case Instrument.Flamingos2 => f2ConfigCodec // how do I prove A =:= Flamingos2Config here?
  //   }

}
