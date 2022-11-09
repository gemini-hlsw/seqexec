// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package ocs2.config

import cats.Eq
import lucuma.core.enums.F2Fpu

/**
 * Additional type hierarchy over the low-level F2 enums.
 */
object F2Config {

  /** Focal plane unit choice, custom or builtin. */
  sealed trait F2FpuChoice extends Product with Serializable {

    import F2FpuChoice.{ Builtin, Custom }

    /** Extracts the builtin FPU, if any. */
    def toBuiltin: Option[F2Fpu] =
      this match {
        case Custom       => None
        case Builtin(fpu) => Some(fpu)
      }
  }

  object F2FpuChoice {
    case object Custom                   extends F2FpuChoice
    final case class Builtin(fpu: F2Fpu) extends F2FpuChoice

    implicit val EqF2FpuChoice: Eq[F2FpuChoice] =
      Eq.fromUniversalEquals
  }

}
