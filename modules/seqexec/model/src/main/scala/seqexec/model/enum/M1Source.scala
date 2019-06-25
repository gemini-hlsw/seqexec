// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import gem.util.Enumerated

/** Enumerated type for M1 Source. */
sealed trait M1Source extends Product with Serializable

object M1Source {
  case object PWFS1 extends M1Source
  case object PWFS2 extends M1Source
  case object OIWFS extends M1Source
  case object GAOS  extends M1Source
  case object HRWFS extends M1Source

  /** All members of M1Source, in canonical order. */
  val all: List[M1Source] =
    List(PWFS1, PWFS2, OIWFS, GAOS, HRWFS)

  def tag(s: M1Source): String = s match {
    case PWFS1 => "PWFS1"
    case PWFS2 => "PWFS2"
    case OIWFS => "OIWFS"
    case GAOS  => "GAOS"
    case HRWFS => "HRWFS"
  }

  /** @group Typeclass Instances */
  implicit val M1SourceEnumerated: Enumerated[M1Source] =
    new Enumerated[M1Source] {
      def all = M1Source.all
      def tag(a: M1Source): String = M1Source.tag(a)
    }

}
