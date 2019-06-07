// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package ocs2

import cats.implicits._
import gem.{ Dataset, EphemerisKey, Observation, Program, Step, Target }
import gem.config._
import gem.enum._
import gem.ocs2.pio.PioPath._
import gem.ocs2.pio.{ PioDecoder, PioError, PioPath }
import gem.ocs2.pio.PioError.ParseError
import gem.ocs2.pio.PioDecoder.fromParse
import gsp.math._
import gsp.math.syntax.all._

import java.time.Instant

import scala.collection.immutable.{ TreeMap, TreeSet }
import scala.xml.Node


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
        rv  = dz.map(RadialVelocity.unsafeFromRedshift)
        dp <- (n \? "#parallax").decode[Double]
        p   = dp.map(d => Angle.fromMicroarcseconds((d * 1000).round))
      } yield ProperMotion(c, e, pv, rv, p)
    }

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

  implicit val TrackDecoder: PioDecoder[Either[EphemerisKey, ProperMotion]] =
    PioDecoder { n =>
      (n \! "#tag").decode[String].flatMap {
        case "nonsidereal" => (n \! "&horizons-designation").decode[EphemerisKey].map(Left(_))
        case "sidereal"    => PioDecoder[ProperMotion].decode(n).map(Right(_))
        case tag           => Left(ParseError(tag, "Track"))
      }
    }


  implicit val TargetDecoder: PioDecoder[Target] =
    PioDecoder { n =>
      for {
        name  <- (n \! "#name").decode[String]
        track <- PioDecoder[Either[EphemerisKey, ProperMotion]].decode(n)
      } yield Target(name, track)
    }

  implicit val UserTargetTypeDecoder: PioDecoder[UserTargetType] =
    fromParse { Parsers.userTargetType }

  implicit val UserTargetDecoder: PioDecoder[UserTarget] =
    PioDecoder { n =>
      for {
        t <- (n \! "&spTarget" \! "&target").decode[Target]
        y <- (n \! "#type"                 ).decode[UserTargetType]
      } yield UserTarget(t, y)
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

  implicit val ObservationIndexDecoder: PioDecoder[Index] =
    PioDecoder { n =>
      (n \! "@name").decode[Observation.Id].map(_.index)
    }

  def targetEnvironmentDecoder(i: Instrument): PioDecoder[TargetEnvironment] = {
    import gem.enum.TrackType

    def trackType(targetNode: scala.xml.Node): Option[TrackType] =
      (targetNode \? "#tag").decode[String].toOption.flatten.collect {
        case "sidereal"    => TrackType.Sidereal: TrackType
        case "nonsidereal" => TrackType.Nonsidereal: TrackType
      }

    // filter out "too" and empty non-sidereal (that is, w/o horizons id) for now
    def validTargets(lst: PioPath.Listing)(f: Node => PioPath.Required): PioPath.Listing =
      lst.copy(node = lst.node.map { ns =>
        ns.filter { n =>

          // We have to drill down to the target node for this filter and
          // extract the track type
          val ty = for {
            t <- f(n).node.toOption // "target" node
            y <- trackType(t)       // track type
          } yield (t, y)

          // Sidereal targets or non-sidereal with an horizons id are ok
          ty.exists { case (t, y) =>
            (y === TrackType.Sidereal) || (t \? "&horizons-designation").node.isDefined.getOrElse(false)
          }
        }
      })

    PioDecoder { n =>
      for {
        a <- validTargets(n \* "&asterism" \! "&target")(_.toRequired).decode[Target]
        // g <- guideEnvironment
        u <- validTargets(n \? "&userTargets" \* "&userTarget")(_ \! "&spTarget" \! "&target").decode[UserTarget]
      } yield {
        a.headOption.map(Asterism.unsafeFromSingleTarget(_, i)) match {
          case None    => TargetEnvironment.fromInstrument(i, TreeSet.fromList(u))
          case Some(a) => TargetEnvironment.fromAsterism(a, TreeSet.fromList(u))
        }
      }
    }
  }

  implicit val ObservationDecoder: PioDecoder[Observation] =
    PioDecoder { n =>
      for {
        t <- (n \! "data" \? "#title"                   ).decodeOrZero[String]
        s <- (n \! "sequence"                           ).decode[StaticConfig](StaticDecoder)
        d <- (n \! "sequence"                           ).decode[List[Step]](SequenceDecoder)
        i  = Instrument.forStaticConfig(s) // stable identifier needed below
        e <- (n \? "telescope" \! "data" \! "&targetEnv").decodeOrElse(TargetEnvironment.fromInstrument(i, TreeSet.empty))(targetEnvironmentDecoder(i))
      } yield Observation.unsafeAssemble(t, e, s, d)
    }

  implicit val ProgramDecoder: PioDecoder[Program] =
    PioDecoder { n =>
      for {
        id <- (n \!  "@name"           ).decode[Program.Id]
        t  <- (n \!  "data" \? "#title").decodeOrZero[String]
        is <- (n \\* "observation"     ).decode[Index]
        os <- (n \\* "observation"     ).decode[Observation]
      } yield Program(id, t, TreeMap.fromList(is.zip(os)))
    }

}
