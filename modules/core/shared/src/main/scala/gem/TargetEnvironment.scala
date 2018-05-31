// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import scala.collection.immutable.TreeSet

/** Collection of targets associated with an observation. */
sealed trait TargetEnvironment {
  def asterism: Option[Asterism]
  def userTargets: TreeSet[UserTarget]
  override final def toString =
    s"TargetEnvironment($asterism, $userTargets)"
}

object TargetEnvironment {

  /** Target environment for Phoenix
    * @group Constructors
    */
  final case class Phoenix(
    asterism: Option[Asterism.Phoenix],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Michelle
    * @group Constructors
    */
  final case class Michelle(
    asterism: Option[Asterism.Michelle],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Gnirs
    * @group Constructors
    */
  final case class Gnirs(
    asterism: Option[Asterism.Gnirs],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Niri
    * @group Constructors
    */
  final case class Niri(
    asterism: Option[Asterism.Niri],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Trecs
    * @group Constructors
    */
  final case class Trecs(
    asterism: Option[Asterism.Trecs],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Nici
    * @group Constructors
    */
  final case class Nici(
    asterism: Option[Asterism.Nici],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Nifs
    * @group Constructors
    */
  final case class Nifs(
    asterism: Option[Asterism.Nifs],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Gpi
    * @group Constructors
    */
  final case class Gpi(
    asterism: Option[Asterism.Gpi],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Gsaoi
    * @group Constructors
    */
  final case class Gsaoi(
    asterism: Option[Asterism.Gsaoi],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for GmosS
    * @group Constructors
    */
  final case class GmosS(
    asterism: Option[Asterism.GmosS],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for AcqCam
    * @group Constructors
    */
  final case class AcqCam(
    asterism: Option[Asterism.AcqCam],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for GmosN
    * @group Constructors
    */
  final case class GmosN(
    asterism: Option[Asterism.GmosN],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Bhros
    * @group Constructors
    */
  final case class Bhros(
    asterism: Option[Asterism.Bhros],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Visitor
    * @group Constructors
    */
  final case class Visitor(
    asterism: Option[Asterism.Visitor],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Flamingos2
    * @group Constructors
    */
  final case class Flamingos2(
    asterism: Option[Asterism.Flamingos2],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Ghost
    * @group Constructors
    */
  final case class Ghost(
    asterism: Option[Asterism.GhostDualTarget],
    userTargets: TreeSet[UserTarget]
  ) extends TargetEnvironment

  implicit def EqTargetEnvironment: Eq[TargetEnvironment] =
    Eq.fromUniversalEquals

}