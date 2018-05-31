// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Order, Show }
import cats.implicits._
import gem.config.StaticConfig
import gem.math.Index

sealed trait Observation {
  def title: String
  def targetEnvironment: TargetEnvironment
  def staticConfig: StaticConfig
  def sequence: List[Step]
}

object Observation {

  final case class Phoenix(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Phoenix,
    sequence: List[Step.Phoenix]
  ) extends Observation

  final case class Michelle(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Michelle,
    sequence: List[Step.Michelle]
  ) extends Observation

  final case class Gnirs(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Gnirs,
    sequence: List[Step.Gnirs]
  ) extends Observation

  final case class Niri(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Niri,
    sequence: List[Step.Niri]
  ) extends Observation

  final case class Trecs(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Trecs,
    sequence: List[Step.Trecs]
  ) extends Observation

  final case class Nici(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Nici,
    sequence: List[Step.Nici]
  ) extends Observation

  final case class Nifs(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Nifs,
    sequence: List[Step.Nifs]
  ) extends Observation

  final case class Gpi(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Gpi,
    sequence: List[Step.Gpi]
  ) extends Observation

  final case class Gsaoi(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Gsaoi,
    sequence: List[Step.Gsaoi]
  ) extends Observation

  final case class GmosS(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.GmosS,
    sequence: List[Step.GmosS]
  ) extends Observation

  final case class AcqCam(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.AcqCam,
    sequence: List[Step.AcqCam]
  ) extends Observation

  final case class GmosN(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.GmosN,
    sequence: List[Step.GmosN]
  ) extends Observation

  final case class Bhros(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Bhros,
    sequence: List[Step.Bhros]
  ) extends Observation

  final case class Visitor(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Visitor,
    sequence: List[Step.Visitor]
  ) extends Observation

  final case class Flamingos2(
    title: String,
    targetEnvironment: TargetEnvironment.DefaultSingleTarget,
    staticConfig: StaticConfig.Flamingos2,
    sequence: List[Step.Flamingos2]
  ) extends Observation

  final case class Ghost(
    title: String,
    targetEnvironment: TargetEnvironment,
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

}
