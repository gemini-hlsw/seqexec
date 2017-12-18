// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import cats.implicits._
import gem.{ Dataset, EphemerisKey, Observation, Program, Step, Target, Track }
import gem.config._
import gem.enum.{ EphemerisKeyType, Instrument, MagnitudeBand, MagnitudeSystem }
import gem.math._
import gem.ocs2.pio.PioPath._
import gem.ocs2.pio.{ PioDecoder, PioError }
import gem.ocs2.pio.PioError.ParseError
import gem.ocs2.pio.PioDecoder.fromParse
import gem.syntax.string._

import java.time.Instant

import scala.collection.immutable.TreeMap


/** `PioDecoder` instances for our model types.
  */
object Decoders {

  implicit val MagnitudeSystemDecoder: PioDecoder[MagnitudeSystem] =
    fromParse { Parsers.magnitudeSystem }

  implicit val MagnitudeBandDecoder: PioDecoder[MagnitudeBand] =
    fromParse { Parsers.magnitudeBand }

  implicit val RightAscensionDecoder: PioDecoder[RightAscension] =
    fromParse { Parsers.ra }

  implicit val DeclinationDecoder: PioDecoder[Declination] =
    fromParse { Parsers.dec }

  implicit val CoordinatesDecoder: PioDecoder[Coordinates] =
    PioDecoder { n =>
      for {
        r <- (n \! "#ra" ).decode[RightAscension]
        d <- (n \! "#dec").decode[Declination]
      } yield Coordinates(r, d)
    }

  implicit val EpochDecoder: PioDecoder[Epoch] =
    fromParse { Parsers.epoch }

  implicit val ProperMotionDecoder: PioDecoder[ProperMotion] =
    PioDecoder { n =>
      for {
        c  <- (n \! "&coordinates").decode[Coordinates]
        e  <- (n \? "&proper-motion" \! "#epoch"    ).decodeOrElse[Epoch](Epoch.J2000)
        dr <- (n \? "&proper-motion" \! "#delta-ra" ).decode[Double]
        dd <- (n \? "&proper-motion" \! "#delta-dec").decode[Double]
        pv  = dr.flatMap { r =>
          dd.map { d =>
            Offset(
              Offset.P(Angle.fromMicroarcseconds((r * 1000).round)),
              Offset.Q(Angle.fromMicroarcseconds((d * 1000).round))
            )
          }
        }
        dz <- (n \? "#redshift").decode[Double]
        rv  = dz.map(RadialVelocity.fromRedshift)
        dp <- (n \? "#parallax").decode[Double]
        p   = dp.map(d => Angle.fromMicroarcseconds((d * 1000).round))
      } yield ProperMotion(c, e, pv, rv, p)
    }

  implicit val SiderealDecoder: PioDecoder[Track.Sidereal] =
    PioDecoder[ProperMotion].map { Track.Sidereal(_) }

  implicit val EphemerisKeyTypeDecoder: PioDecoder[EphemerisKeyType] =
    fromParse { Parsers.ephemerisKeyType }

  implicit val EphemerisKeyDecoder: PioDecoder[EphemerisKey] = {
      def fromDes(des: String, tag: EphemerisKeyType): Either[PioError, EphemerisKey] =
        tag match {
          case EphemerisKeyType.AsteroidNew  =>
            Right(EphemerisKey.AsteroidNew(des))

          case EphemerisKeyType.AsteroidOld  =>
            des.parseIntOption.toRight(PioError.ParseError(des, "AsteroidOld")).map(EphemerisKey.AsteroidOld(_))

          case EphemerisKeyType.Comet        =>
            Right(EphemerisKey.Comet(des))

          case EphemerisKeyType.MajorBody    =>
            des.parseIntOption.toRight(PioError.ParseError(des, "MajorBody")).map(EphemerisKey.MajorBody(_))

          case EphemerisKeyType.UserSupplied =>
            Left(ParseError(des, "EphemerisKey"))
        }

      PioDecoder { n =>
        for {
          des <- (n \! "#des").decode[String]
          tag <- (n \! "#tag").decode[EphemerisKeyType]
          key <- fromDes(des, tag)
        } yield key
      }
    }


  implicit val NonsiderealDecoder: PioDecoder[Track.Nonsidereal] =
    PioDecoder { n =>
      (n \! "&horizons-designation").decode[EphemerisKey].map { key =>
        Track.Nonsidereal(key, Map.empty)
      }
    }

  implicit val TrackDecoder: PioDecoder[Track] =
    PioDecoder { n =>
      (n \! "#tag").decode[String].flatMap {
        case "nonsidereal" => PioDecoder[Track.Nonsidereal].decode(n)
        case "sidereal"    => PioDecoder[Track.Sidereal].decode(n)
        case tag           => Left(ParseError(tag, "Track"))
      }
    }


  implicit val TargetDecoder: PioDecoder[Target] =
    PioDecoder { n =>
      for {
        name  <- (n \! "#name").decode[String]
        track <- PioDecoder[Track].decode(n)
      } yield Target(name, track)
    }

  implicit val DatasetLabelDecoder: PioDecoder[Dataset.Label] =
    fromParse { Parsers.datasetLabel }

  implicit val ObservationIdDecoder: PioDecoder[Observation.Id] =
    fromParse { Parsers.obsId }

  implicit val ProgramIdDecoder: PioDecoder[Program.Id] =
    fromParse { Parsers.progId }

  implicit val InstrumentDecoder: PioDecoder[Instrument] =
    fromParse { Parsers.instrument }

  implicit val DatasetDecoder: PioDecoder[Dataset] =
    PioDecoder { n =>
      for {
        l <- (n \! "#datasetLabel").decode[Dataset.Label]
        f <- (n \! "#dhsFilename" ).decode[String]
        t <- (n \! "#timestamp"   ).decode[Instant]
      } yield Dataset(l, f, t)
    }

  // Decodes all the datasets in either a program or observation node.  We have
  // to explicitly specify that we want the "dataset" elements within a
  // "datasets" paramset because they are duplicated in the event data.
  implicit val DatasetsDecoder: PioDecoder[List[Dataset]] =
    PioDecoder { n =>
      (n \\* "obsExecLog" \\* "&datasets" \\* "&dataset").decode[Dataset]
    }

  implicit val ObservationIndexDecoder: PioDecoder[Observation.Index] =
    PioDecoder { n =>
      (n \! "@name").decode[Observation.Id].map(_.index)
    }

  implicit val ObservationDecoder: PioDecoder[Observation[StaticConfig, Step[DynamicConfig]]] =
    PioDecoder { n =>
      for {
        t  <- (n \! "data" \? "#title").decodeOrZero[String]
        st <- (n \! "sequence"        ).decode[StaticConfig](StaticDecoder)
        sq <- (n \! "sequence"        ).decode[List[Step[DynamicConfig]]](SequenceDecoder)
      } yield Observation(t, st, sq)
    }

  implicit val ProgramDecoder: PioDecoder[Program[Observation[StaticConfig, Step[DynamicConfig]]]] =
    PioDecoder { n =>
      for {
        id <- (n \!  "@name"           ).decode[Program.Id]
        t  <- (n \!  "data" \? "#title").decodeOrZero[String]
        is <- (n \\* "observation"     ).decode[Observation.Index]
        os <- (n \\* "observation"     ).decode[Observation[StaticConfig, Step[DynamicConfig]]]
      } yield Program(id, t, TreeMap(is.zip(os): _*))
    }

  /** Decodes all the program's targets, regardless of where they may be found.
    */
  implicit val TargetsDecoder: PioDecoder[List[Target]] = {

    // (I plan for this to be temporary.  Targets will be associated with
    // observations and read where an observation is read.)

    import gem.enum.TrackType

    def trackType(targetNode: scala.xml.Node): Option[TrackType] =
      (targetNode \? "#tag").decode[String].toOption.flatten.collect {
        case "sidereal"    => TrackType.Sidereal: TrackType
        case "nonsidereal" => TrackType.Nonsidereal: TrackType
      }

    PioDecoder { n =>
      val listing = (n \\* "&target")

      // filter out "too" and empty non-sidereal (that is, w/o horizons id) for now
      val targets = listing.copy(node = listing.node.map { ns =>
        ns.filter { n =>
          trackType(n).fold(false) {
            case TrackType.Sidereal    => true
            case TrackType.Nonsidereal => (n \? "&horizons-designation").node.isDefined.getOrElse(false)
          }
        }
      })

      targets.decode[Target]
    }
  }
}
