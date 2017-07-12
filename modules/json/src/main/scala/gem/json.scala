// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum.GcalArc
import gem.config.GcalConfig.GcalArcs
import gem.math.{ Angle, Offset }

import java.time.Duration
import argonaut._, Argonaut._, ArgonautShapeless._
import scalaz.{ ISet, OneAnd, Order }

// These json codecs are provided for primitive types that have no natural mapping that would
// otherwise be inferred via argonaut-shapeless. For now we'll treat the JSON format as an
// internal concern rather than a public or long-term storage format. If we decide that we *do*
// want to use JSON externally then auto-derivation becomes dangerous because renaming fields
// will cause the serial format to change.
package object json {

  // Angle mapping to signed arcseconds. NOT implicit.
  val AngleMetaAsSignedArcseconds: CodecJson[Angle] =
    CodecJson.derived[BigDecimal]
      .xmap[Angle](
        b => Angle.fromMicroarcseconds(b.underlying.movePointLeft(6).longValue))(
        a => BigDecimal(new java.math.BigDecimal(a.toMicroarcseconds).movePointRight(6)))

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
    CodecJson.derived[String].xmap(Program.Id.unsafeFromString)(_.format)

  implicit val observationIdCodec: CodecJson[Observation.Id] =
    casecodec2(Observation.Id.apply, Observation.Id.unapply)("program-id", "index")

  // OffsetP maps to a signed angle in arcseconds
  implicit val OffsetPCodec: CodecJson[Offset.P] =
    AngleMetaAsSignedArcseconds.xmap(Offset.P(_))(_.toAngle)

  // OffsetQ maps to a signed angle in arcseconds
  implicit val OffsetQCodec: CodecJson[Offset.Q] =
    AngleMetaAsSignedArcseconds.xmap(Offset.Q(_))(_.toAngle)

  // Codec for ISet
  implicit def isetCodec[A: CodecJson: Order]: CodecJson[ISet[A]] =
    CodecJson.derived[List[A]].xmap(ISet.fromList(_))(_.toList)

  // Codec for OneAnd
  implicit def oneAndCodec[F[_], A: CodecJson](implicit ev: CodecJson[F[A]]): CodecJson[OneAnd[F, A]] =
    CodecJson.derived[(A, F[A])].xmap { case (a, fa) => OneAnd(a, fa) } { oa => (oa.head, oa.tail) }

  // Codec for GcalArcs
  implicit def gcalArcsCodec: CodecJson[GcalArcs] =
    CodecJson.derived[OneAnd[ISet, GcalArc]].xmap(oa => GcalArcs(oa.head, oa.tail.toList))(_.arcs)
}
