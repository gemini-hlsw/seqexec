// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl
package low

import cats.effect.IO

/** Low-level constructors for `CtlIO` operations related to system I/O. */
object io {

  final case class Output(exitCode: Int, lines: List[String])

  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
  def exec(cmd: Either[String, List[String]], f: String => IO[Unit]): IO[Output] =
    IO {
      import scala.sys.process._
      import collection.mutable.ListBuffer
      val b = ListBuffer[String]()
      val x = cmd.fold(_.cat, _.cat).run(ProcessLogger { s =>
        f(s).unsafeRunSync // shh
        b.append(s) // side-effect
      }).exitValue
      Output(x, b.toList)
    }

}
