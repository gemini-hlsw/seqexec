package gem.ctl
package low

import scalaz._
import scalaz.effect._

/** Low-level constructors for `CtlIO` operations related to system I/O. */
object io {

  case class Output(exitCode: Int, lines: List[String])

  def exec(cmd: String \/ List[String], f: String => IO[Unit] = (_ => IO.ioUnit)): IO[Output] =
    IO {
      import scala.sys.process._
      import collection.mutable.ListBuffer
      val b = ListBuffer[String]()
      val x = cmd.fold(_.cat, _.cat).run(ProcessLogger { s =>
        f(s).unsafePerformIO // shh
        b.append(s) // side-effect
      }).exitValue
      Output(x, b.toList)
    }

}
