package gem
package telnetd
package command

import net.bmjames.opts.{ intOption, help, metavar, short, long, value, strArgument }
import net.bmjames.opts.types.Parser
import tuco._, Tuco._
import scalaz._, Scalaz._

/** A command that lists programs with matching program id and/or title. */
object ls {

  val num: Parser[Int] = {
    val defaultMax = 100
    intOption(
      help(s"Maximum number of programs to display (default $defaultMax)."),
      metavar("<int>"),
      short('n'),
      long("number"),
      value(defaultMax)
    )
  }

  val pat: Parser[String] =
    strArgument(
      help("A glob-style pattern"),
      metavar("<pattern>"),
      value("*")
    ).map(_.replaceAll("\\*", "%")
           .replaceAll("\\.", "?"))

  val command: GemCommand =
    shellCommand[GemState](
      "ls", "List all visible with ids or titles matching the given pattern.",
      (num |@| pat) { (n, p) => d =>
         for {
           progs  <- d.queryProgramsByName(p, n + 1)
           (h, t)  = progs.splitAt(n)
           cols   <- getColumns
           _      <- formatProgs(h, cols).traverseU(writeLn(_))
           _      <- (t.nonEmpty).whenM(writeLn(s"--Limit reached. ($n)--"))
         } yield d
      }
    ).zoom(Session.L.data[GemState])

  // scala.text and kiama are both on the classpath but neither has any doc so I'm just going
  // to do this by hand for now. We're going to truncate output for now.
  def formatProgs(ps: List[Program[_]], width: Int): List[String] = {
    val w1 = ps.map(_.id.toString.length).foldRight(0)(_ max _) + 2
    val w2 = width - w1
    ps.map { p =>
      p.id.toString.padTo(w1, ' ') + p.title.take(w2)
    }
  }

}
