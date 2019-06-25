// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import gem.util.Enumerated

/** Enumerated type for Tip/Tilt Source. */
sealed trait TipTiltSource extends Product with Serializable

object TipTiltSource {
  case object PWFS1 extends TipTiltSource
  case object PWFS2 extends TipTiltSource
  case object OIWFS extends TipTiltSource
  case object GAOS  extends TipTiltSource

  /** All members of TipTiltSource, in canonical order. */
  val all: List[TipTiltSource] =
    List(PWFS1, PWFS2, OIWFS, GAOS)

  def tag(s: TipTiltSource): String = s match {
    case PWFS1 => "PWFS1"
    case PWFS2 => "PWFS2"
    case OIWFS => "OIWFS"
    case GAOS  => "GAOS"
  }

  /** @group Typeclass Instances */
  implicit val TipTiltSourceEnumerated: Enumerated[TipTiltSource] =
    new Enumerated[TipTiltSource] {
      def all = TipTiltSource.all
      def tag(a: TipTiltSource): String = TipTiltSource.tag(a)
    }
}
