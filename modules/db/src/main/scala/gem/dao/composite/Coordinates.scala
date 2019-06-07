// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gem.dao.meta._

import gsp.math.Coordinates

trait CoordinatesComposite {

  /** Coordinates composite, laid out in natural order, in microarcseconds. */
  implicit val CoordinatesRead: Read[Coordinates] =
    CoordinatesCompositeLemmas.CoordinatesRead

  implicit val CoordinatesOptionRead: Read[Option[Coordinates]] =
    CoordinatesCompositeLemmas.CoordinatesOptionRead

  implicit val CoordinatesWrite: Write[Coordinates] =
    CoordinatesCompositeLemmas.CoordinatesWrite

  implicit val CoordinatesOptionWrite: Write[Option[Coordinates]] =
    CoordinatesCompositeLemmas.CoordinatesOptionWrite

}
object CoordinatesComposite extends CoordinatesComposite

/** Derivation of Composite instances for Coordinates. */
private object CoordinatesCompositeLemmas {
  import DeclinationMeta._
  import RightAscensionMeta._

  val CoordinatesRead: Read[Coordinates] = implicitly
  val CoordinatesOptionRead: Read[Option[Coordinates]] = implicitly

  val CoordinatesWrite: Write[Coordinates] = implicitly
  val CoordinatesOptionWrite: Write[Option[Coordinates]] = implicitly

}
