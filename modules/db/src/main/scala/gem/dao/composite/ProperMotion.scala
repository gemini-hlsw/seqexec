// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gem.dao.meta._

import gem.math._

trait ProperMotionComposite {

  /** ProperMotion composite, laid out in natural order. */
  implicit val ProperMotionComposite: Composite[ProperMotion] =
    ProperMotionCompositeLemmas.ProperMotionComposite

  implicit val ProperMotionOptionComposite: Composite[Option[ProperMotion]] =
    ProperMotionCompositeLemmas.ProperMotionOptionComposite

}
object ProperMotionComposite extends ProperMotionComposite

/** Derivation of Composite instances for ProperMotion. */
private object ProperMotionCompositeLemmas {
  import CoordinatesComposite._
  import EnumeratedMeta._
  import EpochMeta._
  import RadialVelocityMeta._

  // The parallax member is an angle, which we will store as microarcseconds
  private implicit lazy val AngleMeta: Meta[Angle] =
    Meta[Long].xmap(Angle.fromMicroarcseconds, _.toMicroarcseconds)

  val ProperMotionComposite: Composite[ProperMotion] = implicitly
  val ProperMotionOptionComposite: Composite[Option[ProperMotion]] = implicitly

}
