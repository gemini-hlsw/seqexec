// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.imp.TimeInstances._
import java.time.Instant
import scalaz._, Scalaz._

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
          b.drop(1).parseInt.toOption.flatMap { n =>
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
      Order[Observation.Id].contramap[Label](_.observationId) |+|
      Order[Int]           .contramap[Label](_.index)

    /** @group Typeclass Instances */
    implicit val LabelShow: Show[Label] =
      Show.showA

  }

  /**
   * Datasets are ordered by their labels, which are normally unique. For completeness they are further
   * ordered by timestamp and filename.
   * @group Typeclass Instances
   */
  implicit val DatasetOrder: Order[Dataset] =
    Order[Label]  .contramap[Dataset](_.label)     |+|
    Order[Instant].contramap[Dataset](_.timestamp) |+|
    Order[String] .contramap[Dataset](_.filename)

  /** @group Typeclass Instances */
  implicit val DatasetShow: Show[Dataset] =
    Show.showA

}
