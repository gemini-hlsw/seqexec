// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.implicits._
import doobie.implicits._
import gem.Observation
import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._

class ObservationDaoSpec extends PropSpec with PropertyChecks with DaoTest {

  property("ObservationDao should select all observation ids for a program") {
    forAll(genObservationList(pid, limit = 50)) { obsList =>
      val oids = withProgram {
        for {
          _ <- obsList.traverse(o => ObservationDao.insert(o.id, o))
          o <- ObservationDao.selectIds(pid)
        } yield o
      }

      oids.toSet shouldEqual obsList.map(_.id).toSet
    }
  }

  val One: Observation.Index =
    Observation.Index.unsafeFromInt(1)

  property("ObservationDao should select flat observations") {
    val oid = Observation.Id(pid, One)

    forAll(genObservation(oid)) { obsIn =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(oid, obsIn)
          o <- ObservationDao.selectFlat(oid)
        } yield o
      }

      obsOut shouldEqual obsIn.leftMap(_.instrument).copy(steps = Nil)
    }
  }

  property("ObservationDao should select static observations") {
    val oid = Observation.Id(pid, One)

    forAll(genObservation(oid)) { obsIn =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(oid, obsIn)
          o <- ObservationDao.selectStatic(oid)
        } yield o
      }

      obsOut shouldEqual obsIn.copy(steps = Nil)
    }
  }

  property("ObservationDao should roundtrip complete observations") {
    val oid = Observation.Id(pid, One)

    forAll(genObservation(oid)) { obsIn =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(oid, obsIn)
          o <- ObservationDao.select(oid)
        } yield o
      }

      obsOut shouldEqual obsIn
    }
  }

  property("ObservationDao should roundtrip complete observation lists") {
    forAll(genObservationList(pid, limit = 50)) { obsListIn =>
      val obsListOut = withProgram {
        for {
          _ <- obsListIn.traverse(o => ObservationDao.insert(o.id, o))
          o <- ObservationDao.selectAll(pid)
        } yield o.values.toList
      }

      obsListOut shouldEqual obsListIn.sortBy(_.id)
    }
  }

}
