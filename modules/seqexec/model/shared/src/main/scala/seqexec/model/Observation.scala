// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Order
import cats.Show
import lucuma.core.math.Index

object Observation {

  /** An observation is identified by its program and a serial index. */
  final case class Id(pid: Program.Id, index: Index) {
    def format: String =
      s"${ProgramId.fromString.reverseGet(pid)}-${Index.fromString.reverseGet(index)}"
  }
  object Id {

    def fromString(s: String): Option[Observation.Id] =
      s.lastIndexOf('-') match {
        case -1 => None
        case n  =>
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
