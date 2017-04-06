package gem.dao
package check

class ProgramCheck extends Check {
  import ProgramDao.Statements._
  "ProgramDao.Statements" should
            "selectFlat"                    in check(selectFlat(Dummy.programId))
  it should "selectBySubstring"             in check(selectBySubstring("", 0))
  it should "insertArbitraryProgramIdSlice" in check(insertArbitraryProgramIdSlice(Dummy.programId))
  it should "insertDailyProgramIdSlice"     in check(insertDailyProgramIdSlice(Dummy.dailyProgramId))
  it should "insert"                        in check(insert(Dummy.program))
}
