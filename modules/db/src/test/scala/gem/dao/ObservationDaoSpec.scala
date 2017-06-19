package gem.dao

import gem.Observation
import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._


class ObservationDaoSpec extends PropSpec with PropertyChecks with DaoTest {
  property("ObservationDao should roundtrip complete observations") {
    val oid = Observation.Id(pid, 1)

    forAll(genObservation(oid)) { obsIn =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(obsIn)
          o <- ObservationDao.select(oid)
        } yield o
      }

      obsOut shouldEqual obsIn
    }
  }
}