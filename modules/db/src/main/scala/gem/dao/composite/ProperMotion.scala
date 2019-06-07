// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import doobie._
import gem.dao.meta._

import gsp.math.{ Angle, ProperMotion }

trait ProperMotionComposite {

  /** ProperMotion composite, laid out in natural order. */
  implicit val ProperMotionRead: Read[ProperMotion] =
    ProperMotionCompositeLemmas.ProperMotionRead

  implicit val ProperMotionOptionRead: Read[Option[ProperMotion]] =
    ProperMotionCompositeLemmas.ProperMotionOptionRead

  /** ProperMotion composite, laid out in natural order. */
  implicit val ProperMotionWrite: Write[ProperMotion] =
    ProperMotionCompositeLemmas.ProperMotionWrite

  implicit val ProperMotionOptionWrite: Write[Option[ProperMotion]] =
    ProperMotionCompositeLemmas.ProperMotionOptionWrite

}
object ProperMotionComposite extends ProperMotionComposite

/** Derivation of Composite instances for ProperMotion. */
private object ProperMotionCompositeLemmas {
  import CoordinatesComposite._
  import EnumeratedMeta._
  import EpochMeta._
  import RadialVelocityMeta._

  // The parallax member is an angle, which we will store as signed milliarcseconds
  // N.B. if this is marked private we get a spurious "unused" warning
  implicit lazy val AngleMasMeta: Meta[Angle] =
    AngleMeta.AngleMetaAsSignedMilliarcseconds

  val ProperMotionRead: Read[ProperMotion] = implicitly
  val ProperMotionOptionRead: Read[Option[ProperMotion]] = implicitly

  val ProperMotionWrite: Write[ProperMotion] = implicitly
  val ProperMotionOptionWrite: Write[Option[ProperMotion]] = implicitly

}
