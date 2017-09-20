// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gem.dao.meta._

import gem.math._

trait CoordinatesComposite {

  /** Coordinates composite, laid out in natural order, in microarcseconds. */
  implicit val CoordinatesComposite: Composite[Coordinates] =
    CoordinatesCompositeLemmas.CoordinatesComposite

  implicit val CoordinatesOptionComposite: Composite[Option[Coordinates]] =
    CoordinatesCompositeLemmas.CoordinatesOptionComposite

}
object CoordinatesComposite extends CoordinatesComposite

/** Derivation of Composite instances for Coordinates. */
private object CoordinatesCompositeLemmas {
  import DeclinationMeta._
  import RightAscensionMeta._

  val CoordinatesComposite: Composite[Coordinates] = implicitly
  val CoordinatesOptionComposite: Composite[Option[Coordinates]] = implicitly

}
