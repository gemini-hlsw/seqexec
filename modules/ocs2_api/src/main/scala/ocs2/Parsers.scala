// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package ocs2

import cats.syntax.all._
import lucuma.core.enums.GpiObservingMode

/**
 * String parsers for our model types.
 */
object Parsers {

  import gem.ocs2.pio.PioParse
  import gem.ocs2.pio.PioParse._

  object Gpi {

    val observingMode: PioParse[GpiObservingMode] = enumerated(
      GpiObservingMode.all.fproduct(_.longName).map(_.swap): _*
    )

  }
}
