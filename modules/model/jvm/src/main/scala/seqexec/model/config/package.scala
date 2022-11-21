// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import java.nio.file.Path

import cats.Eq

package object config {
  // Used to internally define Eq instances
  private[config] implicit val pathEq: Eq[Path] = Eq.fromUniversalEquals
}
