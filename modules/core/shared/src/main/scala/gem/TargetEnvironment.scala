// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import monocle.macros.Lenses

import scala.collection.immutable.TreeSet

/** Collection of targets associated with an observation.
  */
@Lenses final case class TargetEnvironment(
  /* asterism, */
  /* guide stars, */
  userTargets: TreeSet[UserTarget]
)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object TargetEnvironment {

  val empty: TargetEnvironment =
    TargetEnvironment(TreeSet.empty)

  implicit val EqTargetEnvironment: Eq[TargetEnvironment] =
    Eq.fromUniversalEquals

}