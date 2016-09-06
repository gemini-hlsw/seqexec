package gem
package dao

import doobie.imports._
import java.util.logging.Level

object LogDao {

  private def stack(t: Throwable): String = {
    val sw = new java.io.StringWriter
    val pw = new java.io.PrintWriter(sw)
    t.printStackTrace(pw)
    pw.close
    sw.close
    sw.toString
  }

  def insert(level: Level, pid: Option[Program.Id], msg: String, t: Option[Throwable], elapsed: Option[Long]): ConnectionIO[Int] =
    sql"""
      INSERT INTO log ("timestamp", level, program, message, stacktrace, elapsed)
           VALUES (now(), $level :: log_level, $pid, $msg, ${t.map(stack)}, $elapsed)
     """.update.run

}
