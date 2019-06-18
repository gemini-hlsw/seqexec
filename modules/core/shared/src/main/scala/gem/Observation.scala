// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Eq, Order, Show }
import cats.implicits._
import gem.config.StaticConfig
import gsp.math.Index

/** ADT for an observation, with constructors for each instrument. */
sealed trait Observation {
  def title: String
  def targetEnvironment: TargetEnvironment
  def staticConfig: StaticConfig
  def sequence: List[Step]
}

object Observation {

  /** A Phoenix observation.
    * @group Constructors
    */
  final case class Phoenix(
    title: String,
    targetEnvironment: TargetEnvironment.Phoenix,
    staticConfig: StaticConfig.Phoenix,
    sequence: List[Step.Phoenix]
  ) extends Observation

  /** A Michelle observation.
    * @group Constructors
    */
  final case class Michelle(
    title: String,
    targetEnvironment: TargetEnvironment.Michelle,
    staticConfig: StaticConfig.Michelle,
    sequence: List[Step.Michelle]
  ) extends Observation

  /** A Gnirs observation.
    * @group Constructors
    */
  final case class Gnirs(
    title: String,
    targetEnvironment: TargetEnvironment.Gnirs,
    staticConfig: StaticConfig.Gnirs,
    sequence: List[Step.Gnirs]
  ) extends Observation

  /** A Niri observation.
    * @group Constructors
    */
  final case class Niri(
    title: String,
    targetEnvironment: TargetEnvironment.Niri,
    staticConfig: StaticConfig.Niri,
    sequence: List[Step.Niri]
  ) extends Observation

  /** A Trecs observation.
    * @group Constructors
    */
  final case class Trecs(
    title: String,
    targetEnvironment: TargetEnvironment.Trecs,
    staticConfig: StaticConfig.Trecs,
    sequence: List[Step.Trecs]
  ) extends Observation

  /** A Nici observation.
    * @group Constructors
    */
  final case class Nici(
    title: String,
    targetEnvironment: TargetEnvironment.Nici,
    staticConfig: StaticConfig.Nici,
    sequence: List[Step.Nici]
  ) extends Observation

  /** A Nifs observation.
    * @group Constructors
    */
  final case class Nifs(
    title: String,
    targetEnvironment: TargetEnvironment.Nifs,
    staticConfig: StaticConfig.Nifs,
    sequence: List[Step.Nifs]
  ) extends Observation

  /** A Gpi observation.
    * @group Constructors
    */
  final case class Gpi(
    title: String,
    targetEnvironment: TargetEnvironment.Gpi,
    staticConfig: StaticConfig.Gpi,
    sequence: List[Step.Gpi]
  ) extends Observation

  /** A Gsaoi observation.
    * @group Constructors
    */
  final case class Gsaoi(
    title: String,
    targetEnvironment: TargetEnvironment.Gsaoi,
    staticConfig: StaticConfig.Gsaoi,
    sequence: List[Step.Gsaoi]
  ) extends Observation

  /** A GmosS observation.
    * @group Constructors
    */
  final case class GmosS(
    title: String,
    targetEnvironment: TargetEnvironment.GmosS,
    staticConfig: StaticConfig.GmosS,
    sequence: List[Step.GmosS]
  ) extends Observation

  /** A AcqCam observation.
    * @group Constructors
    */
  final case class AcqCam(
    title: String,
    targetEnvironment: TargetEnvironment.AcqCam,
    staticConfig: StaticConfig.AcqCam,
    sequence: List[Step.AcqCam]
  ) extends Observation

  /** A GmosN observation.
    * @group Constructors
    */
  final case class GmosN(
    title: String,
    targetEnvironment: TargetEnvironment.GmosN,
    staticConfig: StaticConfig.GmosN,
    sequence: List[Step.GmosN]
  ) extends Observation

  /** A Bhros observation.
    * @group Constructors
    */
  final case class Bhros(
    title: String,
    targetEnvironment: TargetEnvironment.Bhros,
    staticConfig: StaticConfig.Bhros,
    sequence: List[Step.Bhros]
  ) extends Observation

  /** A Visitor observation.
    * @group Constructors
    */
  final case class Visitor(
    title: String,
    targetEnvironment: TargetEnvironment.Visitor,
    staticConfig: StaticConfig.Visitor,
    sequence: List[Step.Visitor]
  ) extends Observation

  /** A Flamingos2 observation.
    * @group Constructors
    */
  final case class Flamingos2(
    title: String,
    targetEnvironment: TargetEnvironment.Flamingos2,
    staticConfig: StaticConfig.Flamingos2,
    sequence: List[Step.Flamingos2]
  ) extends Observation

  /** A Ghost observation.
    * @group Constructors
    */
  final case class Ghost(
    title: String,
    targetEnvironment: TargetEnvironment.Ghost,
    staticConfig: StaticConfig.Ghost,
    sequence: List[Step.Ghost]
  ) extends Observation

  /** An observation is identified by its program and a serial index. */
  final case class Id(pid: Program.Id, index: Index) {
    def format: String =
      s"${ProgramId.fromString.reverseGet(pid)}-${Index.fromString.reverseGet(index)}"
  }
  object Id {

    def fromString(s: String): Option[Observation.Id] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          Index.fromString.getOption(b.drop(1)).flatMap { i =>
            Program.Id.fromString.getOption(a).map(Observation.Id(_, i))
          }
      }

    def unsafeFromString(s: String): Observation.Id =
      fromString(s).getOrElse(sys.error("Malformed Observation.Id: " + s))

    /** Observations are ordered by program id and index. */
    implicit val OrderId: Order[Id] =
      Order.by(a => (a.pid, a.index))

    implicit val OrderingId: scala.math.Ordering[Id] =
      OrderId.toOrdering

    implicit val showId: Show[Id] =
      Show.fromToString

  }

  /** Assemble an Observation from its parts, assuming they're consistent. */
  def unsafeAssemble(
    title: String,
    targetEnvironment: TargetEnvironment,
    staticConfig: StaticConfig,
    sequence: List[Step]
  ): Observation = {

    implicit class SequenceOps(ss: List[Step]) {
      def narrow[A](pf: PartialFunction[Step, A]): List[A] =
        ss.collect(pf orElse {
          case _ => sys.error("inconsistent sequence contains multiple instruments")
        })
    }

    (targetEnvironment, staticConfig) match {
        case (te: TargetEnvironment.Phoenix,    sc: StaticConfig.Phoenix)    => Observation.Phoenix(   title, te, sc, sequence.narrow { case s: Step.Phoenix    => s })
        case (te: TargetEnvironment.Michelle,   sc: StaticConfig.Michelle)   => Observation.Michelle(  title, te, sc, sequence.narrow { case s: Step.Michelle   => s })
        case (te: TargetEnvironment.Gnirs,      sc: StaticConfig.Gnirs)      => Observation.Gnirs(     title, te, sc, sequence.narrow { case s: Step.Gnirs      => s })
        case (te: TargetEnvironment.Niri,       sc: StaticConfig.Niri)       => Observation.Niri(      title, te, sc, sequence.narrow { case s: Step.Niri       => s })
        case (te: TargetEnvironment.Trecs,      sc: StaticConfig.Trecs)      => Observation.Trecs(     title, te, sc, sequence.narrow { case s: Step.Trecs      => s })
        case (te: TargetEnvironment.Nici,       sc: StaticConfig.Nici)       => Observation.Nici(      title, te, sc, sequence.narrow { case s: Step.Nici       => s })
        case (te: TargetEnvironment.Nifs,       sc: StaticConfig.Nifs)       => Observation.Nifs(      title, te, sc, sequence.narrow { case s: Step.Nifs       => s })
        case (te: TargetEnvironment.Gpi,        sc: StaticConfig.Gpi)        => Observation.Gpi(       title, te, sc, sequence.narrow { case s: Step.Gpi        => s })
        case (te: TargetEnvironment.Gsaoi,      sc: StaticConfig.Gsaoi)      => Observation.Gsaoi(     title, te, sc, sequence.narrow { case s: Step.Gsaoi      => s })
        case (te: TargetEnvironment.GmosS,      sc: StaticConfig.GmosS)      => Observation.GmosS(     title, te, sc, sequence.narrow { case s: Step.GmosS      => s })
        case (te: TargetEnvironment.AcqCam,     sc: StaticConfig.AcqCam)     => Observation.AcqCam(    title, te, sc, sequence.narrow { case s: Step.AcqCam     => s })
        case (te: TargetEnvironment.GmosN,      sc: StaticConfig.GmosN)      => Observation.GmosN(     title, te, sc, sequence.narrow { case s: Step.GmosN      => s })
        case (te: TargetEnvironment.Bhros,      sc: StaticConfig.Bhros)      => Observation.Bhros(     title, te, sc, sequence.narrow { case s: Step.Bhros      => s })
        case (te: TargetEnvironment.Visitor,    sc: StaticConfig.Visitor)    => Observation.Visitor(   title, te, sc, sequence.narrow { case s: Step.Visitor    => s })
        case (te: TargetEnvironment.Flamingos2, sc: StaticConfig.Flamingos2) => Observation.Flamingos2(title, te, sc, sequence.narrow { case s: Step.Flamingos2 => s })
        case (te: TargetEnvironment.Ghost,      sc: StaticConfig.Ghost)      => Observation.Ghost(     title, te, sc, sequence.narrow { case s: Step.Ghost      => s })
        case _ => sys.error("inconsistent observation")
      }

  }

  implicit val EqObservation: Eq[Observation] =
    Eq.fromUniversalEquals

}
