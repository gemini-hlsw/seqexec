package gem

import doobie.util.capture.Capture
import tuco._, Tuco._

package object telnetd {

  implicit val ConectionIODoobieCapture: Capture[SessionIO] =
    new Capture[SessionIO] {
      def apply[A](a: => A) = SessionIO.delay(a)
    }

}
