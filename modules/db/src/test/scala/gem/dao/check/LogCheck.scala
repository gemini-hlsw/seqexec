// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

import java.util.logging.Level

class LogCheck extends Check {
  import LogDao.Statements._
  "LogDao.Statements" should
            "insert" in check(insert(Dummy.user, Level.INFO, None, "", None, None))
}
