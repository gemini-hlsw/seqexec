// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import monocle.macros.Lenses

/** Collection of targets associated with an observation.
  */
@Lenses final case class TargetEnvironment(
  /* asterism, */
  /* guide stars, */
  userTargets: Set[UserTarget]
)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object TargetEnvironment {

  val empty: TargetEnvironment =
    TargetEnvironment(Set.empty)

  implicit val EqTargetEnvironment: Eq[TargetEnvironment] =
    Eq.fromUniversalEquals

}