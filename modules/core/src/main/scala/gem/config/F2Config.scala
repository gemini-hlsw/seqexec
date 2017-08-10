// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.config

import gem.enum.F2Fpu

import scalaz.Equal

/** Additional type hierarchy over the low-level F2 enums.
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
    case object      Custom              extends F2FpuChoice
    final case class Builtin(fpu: F2Fpu) extends F2FpuChoice

    implicit val EqualF2FpuChoice: Equal[F2FpuChoice] =
      Equal.equalA
  }

}
