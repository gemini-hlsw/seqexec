package gem.dao
package check

class SemesterCheck extends Check {
  import SemesterDao.Statements._
  "SemesterDao.Statements" should
            "canonicalize" in check(canonicalize(Dummy.semester))
}
