// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import gem.enum.Instrument
import scala.collection.immutable.SortedSet

/** Collection of targets associated with an observation. */
sealed trait TargetEnvironment {
  def asterism: Option[Asterism]
  def userTargets: SortedSet[UserTarget]
  override final def toString =
    s"TargetEnvironment($asterism, $userTargets)"
}

object TargetEnvironment {

  def fromAsterism(a: Asterism, u: SortedSet[UserTarget]): TargetEnvironment =
    a match {
      case a @ Asterism.Phoenix(_)            => Phoenix(Some(a), u)
      case a @ Asterism.Michelle(_)           => Michelle(Some(a), u)
      case a @ Asterism.Gnirs(_)              => Gnirs(Some(a), u)
      case a @ Asterism.Niri(_)               => Niri(Some(a), u)
      case a @ Asterism.Trecs(_)              => Trecs(Some(a), u)
      case a @ Asterism.Nici(_)               => Nici(Some(a), u)
      case a @ Asterism.Nifs(_)               => Nifs(Some(a), u)
      case a @ Asterism.Gpi(_)                => Gpi(Some(a), u)
      case a @ Asterism.Gsaoi(_)              => Gsaoi(Some(a), u)
      case a @ Asterism.GmosS(_)              => GmosS(Some(a), u)
      case a @ Asterism.AcqCam(_)             => AcqCam(Some(a), u)
      case a @ Asterism.GmosN(_)              => GmosN(Some(a), u)
      case a @ Asterism.Bhros(_)              => Bhros(Some(a), u)
      case a @ Asterism.Visitor(_)            => Visitor(Some(a), u)
      case a @ Asterism.Flamingos2(_)         => Flamingos2(Some(a), u)
      case a @ Asterism.GhostDualTarget(_, _) => Ghost(Some(a), u)
    }

  def fromInstrument(i: Instrument, ts: SortedSet[UserTarget]): TargetEnvironment =
    i match {
      case Instrument.Phoenix    => TargetEnvironment.Phoenix(None, ts)
      case Instrument.Michelle   => TargetEnvironment.Michelle(None, ts)
      case Instrument.Gnirs      => TargetEnvironment.Gnirs(None, ts)
      case Instrument.Niri       => TargetEnvironment.Niri(None, ts)
      case Instrument.Trecs      => TargetEnvironment.Trecs(None, ts)
      case Instrument.Nici       => TargetEnvironment.Nici(None, ts)
      case Instrument.Nifs       => TargetEnvironment.Nifs(None, ts)
      case Instrument.Gpi        => TargetEnvironment.Gpi(None, ts)
      case Instrument.Gsaoi      => TargetEnvironment.Gsaoi(None, ts)
      case Instrument.GmosS      => TargetEnvironment.GmosS(None, ts)
      case Instrument.AcqCam     => TargetEnvironment.AcqCam(None, ts)
      case Instrument.GmosN      => TargetEnvironment.GmosN(None, ts)
      case Instrument.Bhros      => TargetEnvironment.Bhros(None, ts)
      case Instrument.Visitor    => TargetEnvironment.Visitor(None, ts)
      case Instrument.Flamingos2 => TargetEnvironment.Flamingos2(None, ts)
      case Instrument.Ghost      => TargetEnvironment.Ghost(None, ts)
    }

  /** Target environment for Phoenix
    * @group Constructors
    */
  final case class Phoenix(
    asterism: Option[Asterism.Phoenix],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Michelle
    * @group Constructors
    */
  final case class Michelle(
    asterism: Option[Asterism.Michelle],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Gnirs
    * @group Constructors
    */
  final case class Gnirs(
    asterism: Option[Asterism.Gnirs],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Niri
    * @group Constructors
    */
  final case class Niri(
    asterism: Option[Asterism.Niri],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Trecs
    * @group Constructors
    */
  final case class Trecs(
    asterism: Option[Asterism.Trecs],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Nici
    * @group Constructors
    */
  final case class Nici(
    asterism: Option[Asterism.Nici],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Nifs
    * @group Constructors
    */
  final case class Nifs(
    asterism: Option[Asterism.Nifs],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Gpi
    * @group Constructors
    */
  final case class Gpi(
    asterism: Option[Asterism.Gpi],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Gsaoi
    * @group Constructors
    */
  final case class Gsaoi(
    asterism: Option[Asterism.Gsaoi],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for GmosS
    * @group Constructors
    */
  final case class GmosS(
    asterism: Option[Asterism.GmosS],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for AcqCam
    * @group Constructors
    */
  final case class AcqCam(
    asterism: Option[Asterism.AcqCam],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for GmosN
    * @group Constructors
    */
  final case class GmosN(
    asterism: Option[Asterism.GmosN],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Bhros
    * @group Constructors
    */
  final case class Bhros(
    asterism: Option[Asterism.Bhros],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Visitor
    * @group Constructors
    */
  final case class Visitor(
    asterism: Option[Asterism.Visitor],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Flamingos2
    * @group Constructors
    */
  final case class Flamingos2(
    asterism: Option[Asterism.Flamingos2],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Ghost
    * @group Constructors
    */
  final case class Ghost(
    asterism: Option[Asterism.GhostDualTarget],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  implicit def EqTargetEnvironment: Eq[TargetEnvironment] =
    Eq.fromUniversalEquals

}