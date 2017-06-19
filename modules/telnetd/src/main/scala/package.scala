// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import doobie.util.capture.Capture
import tuco._, Tuco._

package object telnetd {

  /** Capture instance for SessionIO allows us to embed doobie programs. */
  implicit val ConectionIODoobieCapture: Capture[SessionIO] =
    new Capture[SessionIO] {
      def apply[A](a: => A) = SessionIO.delay(a)
    }

}
