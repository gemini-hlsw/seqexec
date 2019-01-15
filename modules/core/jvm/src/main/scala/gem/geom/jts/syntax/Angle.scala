// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.geom.jts.syntax

import gem.math.Angle

// Syntax used in the JVM / JTS implementation only.

final class AngleOps(val self: Angle) extends AnyVal {
  def µas: Long =
    Angle.signedMicroarcseconds.get(self)
}

trait ToAngleOps {
  implicit def ToAngleOps(a: Angle): AngleOps = new AngleOps(a)
}

object angle extends ToAngleOps

