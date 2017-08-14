// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

package object telnetd {

  // implicit conversion from gem lens to tuco lens … all goes away with monocle
  import gem .util.{ Lens => GLens }
  import tuco.util.{ Lens => TLens }
  implicit class gemToTuco[A, B](g: GLens[A, B]) {
    def toTucoLens: TLens[A, B] =
      TLens(g.get, g.set)
  }

}
