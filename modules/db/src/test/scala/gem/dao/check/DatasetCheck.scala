// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao
package check

class DatasetCheck extends Check {
  import DatasetDao.Statements._
  "DatasetDao.Statements" should
            "selectAll" in check(selectAll(Dummy.observationId))
  it should "insert"    in check(insert(0, Dummy.dataset))
}
