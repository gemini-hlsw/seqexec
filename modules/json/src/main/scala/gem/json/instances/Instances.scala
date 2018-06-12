// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package gem.json.instances

// import cats.data.OneAnd
// import gem.User
// import gem.enum.{ GcalArc, ProgramRole }
// import gem.config.GcalConfig.GcalArcs
// import gem.config.GmosConfig._
// import gem.math._
// import gem.optics.Format
// import gem.util._
// import gem.syntax.prism._
// import io.circe._
// import io.circe.syntax._
// import io.circe.generic.semiauto._
// import java.time.{ Duration, Instant }
// import monocle.Prism
// import scala.collection.immutable.TreeMap

// @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
// trait Instances {

//   implicit val coordinatesEncoder: Encoder[Coordinates] = deriveEncoder
//   implicit val coordinatesDecoder: Decoder[Coordinates] = deriveDecoder

//   implicit val offsetEncoder: Encoder[Offset] = deriveEncoder
//   implicit val offsetDecoder: Decoder[Offset] = deriveDecoder

//   implicit val radialVelocityEncoder: Encoder[RadialVelocity] = deriveEncoder
//   implicit val radialVelocityDecoder: Decoder[RadialVelocity] = deriveDecoder

//   implicit val userEncoder: Encoder[User[ProgramRole]] = deriveEncoder
//   implicit val userDecoder: Decoder[User[ProgramRole]] = deriveDecoder

//   private implicit class MoreAngleOps(a: Angle) {
//     private def moveRight(n: Int): BigDecimal =
//       new java.math.BigDecimal(a.toMicroarcseconds).movePointRight(n)

//     def toSignedArcseconds: BigDecimal =
//       moveRight(6)

//     def toSignedMilliarcseconds: BigDecimal =
//       moveRight(3)
//   }
//   private implicit class MoreAngleCompanionOps(comp: Angle.type) {
//     private def moveLeft(b: BigDecimal, n: Int): Angle =
//       comp.fromMicroarcseconds(b.underlying.movePointLeft(n).longValue)

//     def fromSignedArcseconds(b: BigDecimal): Angle =
//       moveLeft(b, 6)

//     def fromSignedMilliarcseconds(b: BigDecimal): Angle =
//       moveLeft(b, 3)
//   }

//   private implicit class PrismOps[A: Encoder: Decoder, B](p: Prism[A, B]) {
//     def toCodec: (Encoder[B], Decoder[B]) = (
//       Encoder[A].contramap(p.reverseGet),
//       Decoder[A].map(p.unsafeGet(_))
//     )
//   }

//   private implicit class FormatOps[A: Encoder: Decoder, B](p: Format[A, B]) {
//     def toCodec: (Encoder[B], Decoder[B]) = (
//       Encoder[A].contramap(p.reverseGet),
//       Decoder[A].map(p.unsafeGet(_))
//     )
//   }

//   // Angle mapping to signed arcseconds. NOT implicit.
//   val AngleAsSignedArcsecondsEncoder: Encoder[Angle] = Encoder[BigDecimal].contramap(_.toSignedArcseconds)
//   val AngleAsSignedArcsecondsDecoder: Decoder[Angle] = Decoder[BigDecimal].map(Angle.fromSignedArcseconds)

//   // Angle mapping to signed milliarcseconds. NOT implicit.
//   val AngleAsSignedMilliarcsecondsEncoder: Encoder[Angle] = Encoder[BigDecimal].contramap(_.toSignedMilliarcseconds)
//   val AngleAsSignedMilliarcsecondsDecoder: Decoder[Angle] = Decoder[BigDecimal].map(Angle.fromSignedMilliarcseconds)

//   // Index to Short.
//   implicit val (
//     observationIndexEncoder: Encoder[Index],
//     observationIndexDecoder: Decoder[Index]
//    ) =
//     Index.fromShort.toCodec

//   // Coadds to Short
//   implicit val (
//     coAddsEncoder: Encoder[CoAdds],
//     coAddsIndexDecoder: Decoder[CoAdds]
//   ) =
//     CoAdds.fromShort.toCodec

//   // Wavelength mapping to integral Angstroms.
//   implicit val (
//     wavelengthEncoderAngstroms: Encoder[Wavelength],
//     wavelengthDecoderAngstroms: Decoder[Wavelength]
//   ) =
//     Wavelength.fromAngstroms.toCodec

//   // Duration as a record with seconds and nanoseconds
//   implicit val DurationEncoder: Encoder[Duration] = d =>
//     Json.obj(
//       "seconds"     -> d.getSeconds.asJson,
//       "nanoseconds" -> d.getNano.asJson
//     )
//   implicit val DurationDecoder: Decoder[Duration] = c =>
//     for {
//       ss <- c.downField("seconds")    .as[Long]
//       ns <- c.downField("nanoseconds").as[Long]
//     } yield Duration.ofSeconds(ss, ns)

//   // Instant as a record with epoch seconds and nanosecond-of-second
//   implicit val InstantEncoder: Encoder[Instant] = i =>
//     Json.obj(
//       "seconds"     -> i.getEpochSecond.asJson,
//       "nanoseconds" -> i.getNano.asJson
//     )
//   implicit val InstantDecoder: Decoder[Instant] = c =>
//     for {
//       ss <- c.downField("seconds")    .as[Long]
//       ns <- c.downField("nanoseconds").as[Long]
//     } yield Instant.ofEpochSecond(ss, ns)

//   // Timestamp
//   implicit val TimestampEncoder: Encoder[Timestamp] = Encoder[Instant].contramap(_.toInstant)
//   implicit val TimestampDecoder: Decoder[Timestamp] = Decoder[Instant].map(Timestamp.unsafeFromInstant)

//   // Enumerated as a tag
//   implicit def enumeratedEncoder[A](implicit ev: Enumerated[A]): Encoder[A] = Encoder[String].contramap(ev.tag)
//   implicit def enumeratedDecoder[A](implicit ev: Enumerated[A]): Decoder[A] = Decoder[String].map(ev.unsafeFromTag)

//   // Program ID in canonical form
//   implicit val (
//     programIdEncoder: Encoder[Program.Id],
//     programIdDecoder: Decoder[Program.Id]
//    ) =
//     Program.Id.fromString.toCodec

//   // Right Ascension in canonical form
//   implicit val (
//     rightAscensionEncoder: Encoder[RightAscension],
//     rightAscensionDecoder: Decoder[RightAscension]
//   ) =
//     RightAscension.fromStringHMS.toCodec

//   // Declination in canonical form
//   implicit val (
//     declinationEncoder: Encoder[Declination],
//     declinationDecoder: Decoder[Declination]
//   ) =
//     Declination.fromStringSignedDMS.toCodec

//   // Epoch in canonical form
//   implicit val (
//     epochEncoder: Encoder[Epoch],
//     epochDecoder: Decoder[Epoch]
//   ) =
//     Epoch.fromString.toCodec

//   // Proper Motion. Made difficult by the parallax, which requires an explicit Angle encoding.
//   @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
//   implicit val ProperMotionEncoder: Encoder[ProperMotion] = p =>
//     Json.obj(
//       "baseCoordinates" -> p.baseCoordinates.asJson,
//       "epoch"           -> p.epoch.asJson,
//       "properVelocity"  -> p.properVelocity.asJson,
//       "radialVelocity"  -> p.radialVelocity.asJson,
//       "parallax"        -> Encoder.encodeOption(AngleAsSignedMilliarcsecondsEncoder)(p.parallax)
//     )
//   @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
//   implicit val ProperMotionDecoder: Decoder[ProperMotion] = c =>
//     for {
//       bc <- c.downField("baseCoordinates").as[Coordinates]
//       ep <- c.downField("epoch")          .as[Epoch]
//       pv <- c.downField("properVelocity") .as[Option[Offset]]
//       rv <- c.downField("radialVelocity") .as[Option[RadialVelocity]]
//       px <- c.downField("parallax")       .as[Option[Angle]](Decoder.decodeOption(AngleAsSignedMilliarcsecondsDecoder))
//     } yield ProperMotion(bc, ep, pv, rv, px)

//   // Offset.P maps to a signed angle in arcseconds
//   implicit val OffsetPEncoder: Encoder[Offset.P] = AngleAsSignedArcsecondsEncoder.contramap(_.toAngle)
//   implicit val OffsetPDecoder: Decoder[Offset.P] = AngleAsSignedArcsecondsDecoder.map(Offset.P.apply)

//   // Offset.Q maps to a signed angle in arcseconds
//   implicit val OffsetQEncoder: Encoder[Offset.Q] = AngleAsSignedArcsecondsEncoder.contramap(_.toAngle)
//   implicit val OffsetQDecoder: Decoder[Offset.Q] = AngleAsSignedArcsecondsDecoder.map(Offset.Q.apply)

//   // Codec for GcalArcs
//   implicit val GcalArcsEncoder: Encoder[GcalArcs] = Encoder[OneAnd[Set, GcalArc]].contramap(_.arcs)
//   implicit val GcalArcsDecoder: Decoder[GcalArcs] = Decoder[OneAnd[Set, GcalArc]].map(oa => GcalArcs(oa.head, oa.tail.toList))

//   // Codec for maps keyed by Program.Id
//   implicit def programIdKeyedMapEncoder[A: Encoder]: Encoder[Map[Program.Id, A]] =
//     Encoder[Map[String, A]].contramap(_.map { case (k, v) => (Program.Id.fromString.reverseGet(k), v) })
//   implicit def programIdKeyedMapDecoder[A: Decoder]: Decoder[Map[Program.Id, A]] =
//     Decoder[Map[String, A]].map(_.map { case (k, v) => (Program.Id.fromString.unsafeGet(k), v) })

//   // Codec for maps keyed by Index
//   implicit def observationIndexMapEncoder[A: Encoder]: Encoder[TreeMap[Index, A]] =
//     Encoder[TreeMap[Short, A]].contramap(_.map { case (k, v) => (k.toShort, v) })
//   implicit def observationIndexMapDecoder[A: Decoder]: Decoder[TreeMap[Index, A]] =
//     Decoder[TreeMap[Short, A]].map(_.map { case (k, v) => (Index.fromShort.unsafeGet(k), v) })

//   // GmosCustomRoiEntry as a quad of shorts
//   implicit val GmosCustomRoiEntryEncoder: Encoder[GmosCustomRoiEntry] =
//     Encoder[(Short, Short, Short, Short)].contramap(r => (r.xMin, r.yMin, r.xRange, r.yRange))
//   implicit val GmosCustomRoiEntryDecoder: Decoder[GmosCustomRoiEntry] =
//     Decoder[(Short, Short, Short, Short)].map((GmosCustomRoiEntry.unsafeFromDescription _).tupled)

//   // GmosShuffleOffset as integer detector rows
//   implicit def GmosShuffleOffsetEncoder: Encoder[GmosShuffleOffset] = Encoder[Int].contramap(_.detectorRows)
//   implicit def GmosShuffleOffsetDecoder: Decoder[GmosShuffleOffset] = Decoder[Int].map(GmosShuffleOffset.unsafeFromRowCount)

//   // GmosShuffleCycles as integer cycle count
//   implicit def GmosShuffleCyclesEncoder: Encoder[GmosShuffleCycles] = Encoder[Int].contramap(_.toInt)
//   implicit def GmosShuffleCyclesDecoder: Decoder[GmosShuffleCycles] = Decoder[Int].map(GmosShuffleCycles.unsafeFromCycleCount)

// }
