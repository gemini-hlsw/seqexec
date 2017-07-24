// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import doobie.imports._
import gem._
import org.scalatest._

class UserDaoSpec extends FlatSpec with Matchers with DaoTest {

  "StepDao" should "select the root user" in {
    val r = UserDao.selectRootUser.transact(xa).unsafePerformIO
    r.id shouldEqual User.Id.Root
  }

}
