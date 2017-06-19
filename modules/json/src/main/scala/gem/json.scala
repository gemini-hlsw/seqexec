/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package gem

import gem.enum.GcalArc
import gem.config.GcalConfig.GcalArcs

import edu.gemini.spModel.core._
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
  implicit val OffsetPCodec: CodecJson[OffsetP] =
    AngleMetaAsSignedArcseconds.xmap(OffsetP(_))(_.toAngle)

  // OffsetQ maps to a signed angle in arcseconds
  implicit val OffsetQCodec: CodecJson[OffsetQ] =
    AngleMetaAsSignedArcseconds.xmap(OffsetQ(_))(_.toAngle)

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
