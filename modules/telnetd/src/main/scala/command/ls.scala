// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd
package command

import cats.implicits._
import com.monovore.decline.{ Command => _, _ }
import tuco._, Tuco._, tuco.shell._

/** A command that lists programs with matching program id and/or title. */
object ls {

  val defaultMax: Int = 100

  val num: Opts[Int] =
    Opts.option[Int](
      help    = s"Maximum number of programs to display (default $defaultMax).",
      metavar = "int",
      short   = "n",
      long    = "number"
    ).withDefault(defaultMax)

  val pat: Opts[String] =
    Opts.argument[String](
      metavar = "pattern"
    ).withDefault("*")
     .map(_.replaceAll("\\*", "%")
           .replaceAll("\\.", "?"))

  val command: GemCommand =
    Command(
      "ls", "List all visible with ids or titles matching the given pattern.",
      (num, pat).mapN { (n: Int, p: String) => (d: GemState) =>
         for {
           progs  <- d.queryProgramsByName(p, n + 1)
           (h, t)  = progs.splitAt(n)
           cols   <- getColumns
           _      <- formatProgs(h, cols).traverse(writeLn(_))
           _      <- writeLn(s"--Limit reached. ($n)--").whenA(t.nonEmpty)
         } yield d
      }
    ).zoom(Session.data[GemState])

  // scala.text and kiama are both on the classpath but neither has any doc so I'm just going
  // to do this by hand for now. We're going to truncate output for now.
  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def formatProgs(ps: List[(Program.Id, String)], width: Int): List[String] = {
    val w1 = ps.map(_._1.toString.length).foldRight(0)(_ max _) + 2
    val w2 = width - w1
    ps.map { p =>
      p._1.toString.padTo(w1, ' ') + p._2.take(w2)
    }
  }

}
