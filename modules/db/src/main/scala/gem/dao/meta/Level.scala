// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import java.util.logging.Level

trait LevelMeta {

  // Java Log Levels (not nullable)
  implicit val levelMeta: Meta[Level] =
    Meta[String].timap(Level.parse)(_.getName)

}
object LevelMeta extends LevelMeta
