// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Order, Show }
import cats.implicits._
import gem.imp.TimeInstances._
import gem.syntax.string._
import java.time.Instant

/**
 * A labeled, timestamped data file.
 * @group Sequence Model
 */
final case class Dataset(
  label:     Dataset.Label,
  filename:  String,
  timestamp: Instant
)

object Dataset {

  /**
   * Datasets are labeled by observation and index.
   * @group Data Types
   */
  final case class Label(observationId: Observation.Id, index: Int) {
    def format: String =
      f"${observationId.format}-$index%03d"
  }
  object Label {

    /** @group Constructors */
    def fromString(s: String): Option[Dataset.Label] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          b.drop(1).parseIntOption.flatMap { n =>
            Observation.Id.fromString(a).map(oid => Dataset.Label(oid, n))
          }
      }

    /** @group Constructors */
    def unsafeFromString(s: String): Dataset.Label =
      fromString(s).getOrElse(sys.error("Malformed Dataset.Label: " + s))

    /**
     * Labels are ordered by observation and index.
     * @group Typeclass Instances
     */
    implicit val LabelOrder: Order[Label] =
      Order.by(a => (a.observationId, a.index))

    /** @group Typeclass Instances */
    implicit val LabelShow: Show[Label] =
      Show.fromToString

  }

  /**
   * Datasets are ordered by their labels, which are normally unique. For completeness they are further
   * ordered by timestamp and filename.
   * @group Typeclass Instances
   */
  implicit val DatasetOrder: Order[Dataset] =
    Order.by(a => (a.label, a.timestamp, a.filename))

  /** @group Typeclass Instances */
  implicit val DatasetShow: Show[Dataset] =
    Show.fromToString

}
