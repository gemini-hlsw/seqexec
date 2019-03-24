// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import doobie._, doobie.implicits._
import java.util.logging.Level
import gem.dao.meta._

object LogDao {
  import LevelMeta._
  import ProgramIdMeta._

  def insert(user: User[_], level: Level, pid: Option[Program.Id], msg: String, t: Option[Throwable], elapsed: Option[Long]): ConnectionIO[Int] =
    Statements.insert(user, level, pid, msg, t, elapsed).run

  object Statements {

    def insert(user: User[_], level: Level, pid: Option[Program.Id], msg: String, t: Option[Throwable], elapsed: Option[Long]): Update0 =
      sql"""
        INSERT INTO log (user_id, "timestamp", level, program, message, stacktrace, elapsed)
             VALUES (${user.id}, now(), $level :: log_level, $pid, $msg, ${t.map(stack)}, $elapsed)
       """.update

    private def stack(t: Throwable): String = {
      val sw = new java.io.StringWriter
      val pw = new java.io.PrintWriter(sw)
      t.printStackTrace(pw)
      pw.close
      sw.close
      sw.toString
    }

  }

}
