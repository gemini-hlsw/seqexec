// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import gem.enum.Instrument
import scala.collection.immutable.SortedSet

/** Collection of targets associated with an observation. */
sealed trait TargetEnvironment {
  def asterism: Option[Asterism]
  def guideEnvironment: GuideEnvironment
  def userTargets: SortedSet[UserTarget]
  override final def toString =
    s"TargetEnvironment($asterism, $userTargets)"
}

object TargetEnvironment {

  def fromAsterism(
    a: Asterism,
    g: Option[GuideEnvironment],
    u: SortedSet[UserTarget]
  ): TargetEnvironment =
    a match {
      case a @ Asterism.AcqCam(_)             => AcqCam(Some(a), u)
      case a @ Asterism.Bhros(_)              => Bhros(Some(a), u)
      case a @ Asterism.Flamingos2(_)         => Flamingos2(Some(a), GuideEnvironment.Flamingos2.typed(g), u)
      case a @ Asterism.GhostDualTarget(_, _) => Ghost(Some(a), u)
      case a @ Asterism.GmosN(_)              => GmosN(Some(a), GuideEnvironment.GmosNorth.typed(g), u)
      case a @ Asterism.GmosS(_)              => GmosS(Some(a), GuideEnvironment.GmosSouth.typed(g), u)
      case a @ Asterism.Gnirs(_)              => Gnirs(Some(a), u)
      case a @ Asterism.Gpi(_)                => Gpi(Some(a), u)
      case a @ Asterism.Gsaoi(_)              => Gsaoi(Some(a), u)
      case a @ Asterism.Michelle(_)           => Michelle(Some(a), u)
      case a @ Asterism.Nici(_)               => Nici(Some(a), u)
      case a @ Asterism.Nifs(_)               => Nifs(Some(a), u)
      case a @ Asterism.Niri(_)               => Niri(Some(a), u)
      case a @ Asterism.Phoenix(_)            => Phoenix(Some(a), u)
      case a @ Asterism.Trecs(_)              => Trecs(Some(a), u)
      case a @ Asterism.Visitor(_)            => Visitor(Some(a), u)
    }

  def fromInstrument(
    i: Instrument,
    g: Option[GuideEnvironment],
    u: SortedSet[UserTarget]
  ): TargetEnvironment =
    i match {
      case Instrument.AcqCam     => AcqCam(None, u)
      case Instrument.Bhros      => Bhros(None, u)
      case Instrument.Flamingos2 => Flamingos2(None, GuideEnvironment.Flamingos2.typed(g), u)
      case Instrument.Ghost      => Ghost(None, u)
      case Instrument.GmosN      => GmosN(None, GuideEnvironment.GmosNorth.typed(g), u)
      case Instrument.GmosS      => GmosS(None, GuideEnvironment.GmosSouth.typed(g), u)
      case Instrument.Gnirs      => Gnirs(None, u)
      case Instrument.Gpi        => Gpi(None, u)
      case Instrument.Gsaoi      => Gsaoi(None, u)
      case Instrument.Michelle   => Michelle(None, u)
      case Instrument.Nici       => Nici(None, u)
      case Instrument.Nifs       => Nifs(None, u)
      case Instrument.Niri       => Niri(None, u)
      case Instrument.Phoenix    => Phoenix(None, u)
      case Instrument.Trecs      => Trecs(None, u)
      case Instrument.Visitor    => Visitor(None, u)
    }

  /** Target environment for Phoenix
    * @group Constructors
    */
  final case class Phoenix(
    asterism: Option[Asterism.Phoenix],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Michelle
    * @group Constructors
    */
  final case class Michelle(
    asterism: Option[Asterism.Michelle],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Gnirs
    * @group Constructors
    */
  final case class Gnirs(
    asterism: Option[Asterism.Gnirs],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Niri
    * @group Constructors
    */
  final case class Niri(
    asterism: Option[Asterism.Niri],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Trecs
    * @group Constructors
    */
  final case class Trecs(
    asterism: Option[Asterism.Trecs],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Nici
    * @group Constructors
    */
  final case class Nici(
    asterism: Option[Asterism.Nici],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Nifs
    * @group Constructors
    */
  final case class Nifs(
    asterism: Option[Asterism.Nifs],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Gpi
    * @group Constructors
    */
  final case class Gpi(
    asterism: Option[Asterism.Gpi],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Gsaoi
    * @group Constructors
    */
  final case class Gsaoi(
    asterism: Option[Asterism.Gsaoi],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for GmosS
    * @group Constructors
    */
  final case class GmosS(
    asterism: Option[Asterism.GmosS],
    guideEnvironment: GuideEnvironment.GmosSouth,
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for AcqCam
    * @group Constructors
    */
  final case class AcqCam(
    asterism: Option[Asterism.AcqCam],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for GmosN
    * @group Constructors
    */
  final case class GmosN(
    asterism: Option[Asterism.GmosN],
    guideEnvironment: GuideEnvironment.GmosNorth,
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Bhros
    * @group Constructors
    */
  final case class Bhros(
    asterism: Option[Asterism.Bhros],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Visitor
    * @group Constructors
    */
  final case class Visitor(
    asterism: Option[Asterism.Visitor],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  /** Target environment for Flamingos2
    * @group Constructors
    */
  final case class Flamingos2(
    asterism: Option[Asterism.Flamingos2],
    guideEnvironment: GuideEnvironment.Flamingos2,
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment

  /** Target environment for Ghost
    * @group Constructors
    */
  final case class Ghost(
    asterism: Option[Asterism.GhostDualTarget],
    userTargets: SortedSet[UserTarget]
  ) extends TargetEnvironment {
    def guideEnvironment: GuideEnvironment =
      GuideEnvironment.NoGuiding
  }

  implicit def EqTargetEnvironment: Eq[TargetEnvironment] =
    Eq.fromUniversalEquals

}