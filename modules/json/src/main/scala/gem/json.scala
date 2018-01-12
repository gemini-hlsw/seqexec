// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.data.OneAnd
import gem.enum.{ GcalArc, Site }
import gem.config.GcalConfig.GcalArcs
import gem.config.GmosConfig._
import gem.math._
import gem.util.{ Enumerated, InstantMicros }
import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import java.time.{ Duration, Instant }
import scala.collection.immutable.TreeMap

// These json codecs are provided for primitive types that have no natural mapping that would
// otherwise be inferred via argonaut-shapeless. For now we'll treat the JSON format as an
// internal concern rather than a public or long-term storage format. If we decide that we *do*
// want to use JSON externally then auto-derivation becomes dangerous because renaming fields
// will cause the serial format to change.
package object json {

  private implicit class MoreAngleOps(a: Angle) {
    private def moveRight(n: Int): BigDecimal =
      new java.math.BigDecimal(a.toMicroarcseconds).movePointRight(n)

    def toSignedArcseconds: BigDecimal =
      moveRight(6)

    def toSignedMilliarcseconds: BigDecimal =
      moveRight(3)
  }
  private implicit class MoreAngleCompanionOps(comp: Angle.type) {
    private def moveLeft(b: BigDecimal, n: Int): Angle =
      comp.fromMicroarcseconds(b.underlying.movePointLeft(n).longValue)

    def fromSignedArcseconds(b: BigDecimal): Angle =
      moveLeft(b, 6)

    def fromSignedMilliarcseconds(b: BigDecimal): Angle =
      moveLeft(b, 3)
  }

  // Angle mapping to signed arcseconds. NOT implicit.
  val AngleAsSignedArcsecondsEncoder: Encoder[Angle] = Encoder[BigDecimal].contramap(_.toSignedArcseconds)
  val AngleAsSignedArcsecondsDecoder: Decoder[Angle] = Decoder[BigDecimal].map(Angle.fromSignedArcseconds)

  // Angle mapping to signed milliarcseconds. NOT implicit.
  val AngleAsSignedMilliarcsecondsEncoder: Encoder[Angle] = Encoder[BigDecimal].contramap(_.toSignedMilliarcseconds)
  val AngleAsSignedMilliarcsecondsDecoder: Decoder[Angle] = Decoder[BigDecimal].map(Angle.fromSignedMilliarcseconds)

  // Observation.Index to Integer.
  implicit val ObservationIndexEncoder: Encoder[Observation.Index] = Encoder[Int].contramap(_.toInt)
  implicit val ObservationIndexDecoder: Decoder[Observation.Index] = Decoder[Int].map(Observation.Index.unsafeFromInt)

  // Wavelength mapping to integral Angstroms.
  implicit val WavelengthEncoder: Encoder[Wavelength] = Encoder[Int].contramap(_.toAngstroms)
  implicit val WavelengthDecoder: Decoder[Wavelength] = Decoder[Int].map(Wavelength.unsafeFromAngstroms)

  // Duration as a record with seconds and nanoseconds
  implicit val DurationEncoder: Encoder[Duration] = d =>
    Json.obj(
      "seconds"     -> d.getSeconds.asJson,
      "nanoseconds" -> d.getNano.asJson
    )
  implicit val DurationDecoder: Decoder[Duration] = c =>
    for {
      ss <- c.downField("seconds")    .as[Long]
      ns <- c.downField("nanoseconds").as[Long]
    } yield Duration.ofSeconds(ss, ns)

  // Instant as a record with epoch seconds and nanosecond-of-second
  implicit val InstantEncoder: Encoder[Instant] = i =>
    Json.obj(
      "seconds"     -> i.getEpochSecond.asJson,
      "nanoseconds" -> i.getNano.asJson
    )
  implicit val InstantDecoder: Decoder[Instant] = c =>
    for {
      ss <- c.downField("seconds")    .as[Long]
      ns <- c.downField("nanoseconds").as[Long]
    } yield Instant.ofEpochSecond(ss, ns)

  // Instant micros
  implicit val InstantMicrosEncoder: Encoder[InstantMicros] = Encoder[Instant].contramap(_.toInstant)
  implicit val InstantMicrosDecoder: Decoder[InstantMicros] = Decoder[Instant].map(InstantMicros.truncate)

  // Enumerated as a tag
  implicit def enumeratedEncoder[A](implicit ev: Enumerated[A]): Encoder[A] = Encoder[String].contramap(ev.tag)
  implicit def enumeratedDecoder[A](implicit ev: Enumerated[A]): Decoder[A] = Decoder[String].map(ev.unsafeFromTag)

  // Program ID in canonical form
  implicit val ProgramIdEncoder: Encoder[Program.Id] = Encoder[String].contramap(_.format)
  implicit val ProgramIdDecoder: Decoder[Program.Id] = Decoder[String].map(Program.Id.unsafeFromString)

  // Right Ascension in canonical form
  implicit def RightAscensionEncoder: Encoder[RightAscension] = Encoder[String].contramap(_.format)
  implicit def RightAscensionDecoder: Decoder[RightAscension] = Decoder[String].map(s => RightAscension.parse(s).getOrElse(sys.error(s"Could not parse '$s' as an RA")))

  // Declination in canonical form
  implicit def DeclinationEncoder: Encoder[Declination] = Encoder[String].contramap(_.format)
  implicit def DeclinationDecoder: Decoder[Declination] = Decoder[String].map(s => Declination.parse(s).getOrElse(sys.error(s"Could not parse '$s' as a declination")))

  // Epoch in canonical form
  implicit def EpochEncoder: Encoder[Epoch] = Encoder[String].contramap(_.format)
  implicit def EpochDecoder: Decoder[Epoch] = Decoder[String].map(Epoch.unsafeFromString)

  // Proper Motion. Made difficult by the parallax, which requires an explicit Angle encoding.
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val ProperMotionEncoder: Encoder[ProperMotion] = p =>
    Json.obj(
      "baseCoordinates" -> p.baseCoordinates.asJson,
      "epoch"           -> p.epoch.asJson,
      "properVelocity"  -> p.properVelocity.asJson,
      "radialVelocity"  -> p.radialVelocity.asJson,
      "parallax"        -> Encoder.encodeOption(AngleAsSignedMilliarcsecondsEncoder)(p.parallax)
    )
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val ProperMotionDecoder: Decoder[ProperMotion] = c =>
    for {
      bc <- c.downField("baseCoordinates").as[Coordinates]
      ep <- c.downField("epoch")          .as[Epoch]
      pv <- c.downField("properVelocity") .as[Option[Offset]]
      rv <- c.downField("radialVelocity") .as[Option[RadialVelocity]]
      px <- c.downField("parallax")       .as[Option[Angle]](Decoder.decodeOption(AngleAsSignedMilliarcsecondsDecoder))
    } yield ProperMotion(bc, ep, pv, rv, px)

  // Ephemeris as a List[Ephemeris.Element]
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val EphemerisEncoder: Encoder[Ephemeris] = Encoder[List[Ephemeris.Element]].contramap(_.toMap.toList)
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val EphemerisDecoder: Decoder[Ephemeris] = Decoder[List[Ephemeris.Element]].map(ls => Ephemeris(ls: _*))

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val NonsiderealEncoder: Encoder[Track.Nonsidereal] = n =>
    Json.obj(
      "key"         -> n.ephemerisKey.asJson,
      "ephemerides" -> n.ephemerides.toList.asJson
    )
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val NonsiderealDecoder: Decoder[Track.Nonsidereal] = c =>
    for {
      k <- c.downField("key").as[EphemerisKey]
      e <- c.downField("ephemerides").as[List[(Site, Ephemeris)]].map(_.toMap)
    } yield Track.Nonsidereal(k, e)

  // Offset.P maps to a signed angle in arcseconds
  implicit val OffsetPEncoder: Encoder[Offset.P] = AngleAsSignedArcsecondsEncoder.contramap(_.toAngle)
  implicit val OffsetPDecoder: Decoder[Offset.P] = AngleAsSignedArcsecondsDecoder.map(Offset.P.apply)

  // Offset.Q maps to a signed angle in arcseconds
  implicit val OffsetQEncoder: Encoder[Offset.Q] = AngleAsSignedArcsecondsEncoder.contramap(_.toAngle)
  implicit val OffsetQDecoder: Decoder[Offset.Q] = AngleAsSignedArcsecondsDecoder.map(Offset.Q.apply)

  // Codec for GcalArcs
  implicit val GcalArcsEncoder: Encoder[GcalArcs] = Encoder[OneAnd[Set, GcalArc]].contramap(_.arcs)
  implicit val GcalArcsDecoder: Decoder[GcalArcs] = Decoder[OneAnd[Set, GcalArc]].map(oa => GcalArcs(oa.head, oa.tail.toList))

  // Codec for maps keyed by Program.Id
  implicit def programIdKeyedMapEncoder[A: Encoder]: Encoder[Map[Program.Id, A]] =
    Encoder[Map[String, A]].contramap(_.map { case (k, v) => (k.format, v) })
  implicit def programIdKeyedMapDecoder[A: Decoder]: Decoder[Map[Program.Id, A]] =
    Decoder[Map[String, A]].map(_.map { case (k, v) => (Program.Id.unsafeFromString(k), v) })

  // Codec for maps keyed by Observation.Index
  implicit def observationIndexMapEncoder[A: Encoder]: Encoder[TreeMap[Observation.Index, A]] =
    Encoder[TreeMap[Int, A]].contramap(_.map { case (k, v) => (k.toInt, v) })
  implicit def observationIndexMapDecoder[A: Decoder]: Decoder[TreeMap[Observation.Index, A]] =
    Decoder[TreeMap[Int, A]].map(_.map { case (k, v) => (Observation.Index.unsafeFromInt(k), v) })

  // GmosCustomRoiEntry as a quad of shorts
  implicit val GmosCustomRoiEntryEncoder: Encoder[GmosCustomRoiEntry] =
    Encoder[(Short, Short, Short, Short)].contramap(r => (r.xMin, r.yMin, r.xRange, r.yRange))
  implicit val GmosCustomRoiEntryDecoder: Decoder[GmosCustomRoiEntry] =
    Decoder[(Short, Short, Short, Short)].map((GmosCustomRoiEntry.unsafeFromDescription _).tupled)

  // GmosShuffleOffset as integer detector rows
  implicit def GmosShuffleOffsetEncoder: Encoder[GmosShuffleOffset] = Encoder[Int].contramap(_.detectorRows)
  implicit def GmosShuffleOffsetDecoder: Decoder[GmosShuffleOffset] = Decoder[Int].map(GmosShuffleOffset.unsafeFromRowCount)

  // GmosShuffleCycles as integer cycle count
  implicit def GmosShuffleCyclesEncoder: Encoder[GmosShuffleCycles] = Encoder[Int].contramap(_.toInt)
  implicit def GmosShuffleCyclesDecoder: Decoder[GmosShuffleCycles] = Decoder[Int].map(GmosShuffleCycles.unsafeFromCycleCount)

}
