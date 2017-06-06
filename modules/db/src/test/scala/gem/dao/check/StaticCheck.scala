package gem.dao
package check

import gem.enum.Instrument.Flamingos2

class StaticCheck extends Check {
  import StaticConfigDao.Statements._

  "StaticDao.Statements" should
            "selectF2"        in check(selectF2(0))
  it should "insertBaseSlice" in check(insertBaseSlice(Flamingos2))
  it should "insertF2"        in check(insertF2(0, Dummy.f2Static))
}
