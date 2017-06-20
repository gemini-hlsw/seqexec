// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import java.time.Instant

import scalaz._, Scalaz._

final case class Dataset(
  label: Dataset.Label,
  filename: String,
  timestamp: Instant)

object Dataset {

  final case class Label(oid: Observation.Id, index: Int) {
    override def toString = f"$oid-$index%03d"
  }

  object Label {
    def fromString(s: String): Option[Dataset.Label] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          b.drop(1).parseInt.toOption.flatMap { n =>
            Observation.Id.fromString(a).map(oid => Dataset.Label(oid, n))
          }
      }

    def unsafeFromString(s: String): Dataset.Label =
      fromString(s).getOrElse(sys.error("Malformed Dataset.Label: " + s))
  }

}
