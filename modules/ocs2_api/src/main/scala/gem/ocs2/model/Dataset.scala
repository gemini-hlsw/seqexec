// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package ocs2

import cats.{ Order, Show }
import io.chrisdavenport.cats.time.instances.instant.instantInstances
import seqexec.model.Observation
import lucuma.core.syntax.string._
import lucuma.core.optics.Format
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
    override def toString =
      Label.fromString.productToString(this)
  }

  object Label extends LabelOptics {

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

  trait LabelOptics { this: Label.type =>

    /** Format from Strings into Label and back. */
    val fromString: Format[String, Label] =
      Format(
        s =>
          s.lastIndexOf('-') match {
            case -1 => None
            case n  =>
              val (a, b) = s.splitAt(n)
              b.drop(1).parseIntOption.filter(_ > 0).flatMap { n =>
                Observation.Id.fromString(a).map(oid => Dataset.Label(oid, n))
              }
          },
        l => f"${l.observationId.format}-${l.index}%03d"
      )

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
