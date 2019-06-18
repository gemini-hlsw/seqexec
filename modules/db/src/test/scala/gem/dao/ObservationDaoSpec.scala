// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.implicits._
import doobie.util.invariant.UnexpectedEnd
import doobie.implicits._
import gem.Observation
import gem.enum._
import gsp.math.Index
import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._

class ObservationDaoSpec extends PropSpec with PropertyChecks with DaoTest {

  import gem.arb.ArbObservation._

  val Obs1: Observation.Id = Observation.Id(pid, Index.One)

  property("ObservationDao should select all observation ids for a program") {
    forAll(genObservationMap(limit = 50)) { obsMap =>
      val oids = withProgram {
        for {
          _ <- obsMap.toList.traverse { case (i,o) => ObservationDao.insert(Observation.Id(pid, i), o) }
          o <- ObservationDao.queryIds(pid)
        } yield o
      }

      oids.toSet shouldEqual obsMap.keys.map(idx => Observation.Id(pid, idx)).toSet
    }
  }

  property("ObservationDao should fetchFlat") {
    forAll { (obsIn: Observation) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(Obs1, obsIn)
          o <- ObservationDao.fetchFlat(Obs1)
        } yield o
      }

      // Take the generated observation, remove the targets and steps, and map
      // the the static config to the instrument.
      val expected = (obsIn.title, Instrument.forObservation(obsIn))

      obsOut shouldEqual expected
    }
  }

  property("ObservationDao should fail fetchFlat if missing") {
    assertThrows[UnexpectedEnd.type] {
      withProgram(ObservationDao.fetchFlat(Obs1))
    }
  }

  property("ObservationDao should queryFlat") {
    forAll { (obsIn: Observation) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(Obs1, obsIn)
          o <- ObservationDao.queryFlat(Obs1)
        } yield o
      }

      // Take the generated observation, remove the targets and steps, and map
      // the the static config to the instrument.
      val expected = (obsIn.title, Instrument.forObservation(obsIn))

      obsOut shouldEqual Some(expected)
    }
  }

  property("ObservationDao should queryFlat missing observations") {
    withProgram(ObservationDao.queryFlat(Obs1)) shouldEqual None
  }

  property("ObservationDao should fetchStatic") {
    forAll { (obsIn: Observation) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(Obs1, obsIn)
          o <- ObservationDao.fetchStatic(Obs1)
        } yield o
      }

      // Take the generated observation and remove the targets and steps
      val expected = (obsIn.title, obsIn.staticConfig)

      obsOut shouldEqual expected
    }
  }

  property("ObservationDao should fail fetchStatic if missing") {
    assertThrows[UnexpectedEnd.type] {
      withProgram(ObservationDao.fetchStatic(Obs1))
    }
  }

  property("ObservationDao should queryStatic") {
    forAll { (obsIn: Observation) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(Obs1, obsIn)
          o <- ObservationDao.queryStatic(Obs1)
        } yield o
      }

      // Take the generated observation and remove the targets and steps
      val expected = (obsIn.title, obsIn.staticConfig)

      obsOut shouldEqual Some(expected)
    }
  }

  property("ObservationDao should queryStatic missing observations") {
    withProgram(ObservationDao.queryStatic(Obs1)) shouldEqual None
  }


  property("ObservationDao should fetchTargets") {
    forAll { (obsIn: Observation) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(Obs1, obsIn)
          o <- ObservationDao.fetchTargets(Obs1)
        } yield o
      }

      // Take the generated observation, replace the static config with the
      // instrument type and remove the steps.
      val expected = (obsIn.title, obsIn.targetEnvironment)

      obsOut shouldEqual expected
    }
  }

  property("ObservationDao should fail fetchTargets if missing") {
    assertThrows[UnexpectedEnd.type] {
      withProgram(ObservationDao.fetchTargets(Obs1))
    }
  }

  property("ObservationDao should queryTargets") {
    forAll { (obsIn: Observation) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(Obs1, obsIn)
          o <- ObservationDao.queryTargets(Obs1)
        } yield o
      }

      // Take the generated observation, replace the static config with the
      // instrument type and remove the steps.
      val expected = (obsIn.title, obsIn.targetEnvironment)

      obsOut shouldEqual Some(expected)
    }
  }

  property("ObservationDao should queryTargets missing observations") {
    withProgram(ObservationDao.queryTargets(Obs1)) shouldEqual None
  }

  property("ObservationDao should fail fetch if missing") {
    assertThrows[UnexpectedEnd.type] {
      withProgram(ObservationDao.fetch(Obs1))
    }
  }

  property("ObservationDao should query missing observations") {
    withProgram(ObservationDao.query(Obs1)) shouldEqual None
  }


  property("ObservationDao should roundtrip complete observations with fetch") {
    forAll { (obsIn: Observation) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(Obs1, obsIn)
          o <- ObservationDao.fetch(Obs1)
        } yield o
      }

      obsOut shouldEqual obsIn
    }
  }

  property("ObservationDao should roundtrip complete observations with query") {
    forAll { (obsIn: Observation) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(Obs1, obsIn)
          o <- ObservationDao.query(Obs1)
        } yield o
      }

      obsOut shouldEqual Some(obsIn)
    }
  }

  property("ObservationDao should roundtrip complete observation lists") {
    forAll(genObservationMap(limit = 50)) { obsMapIn =>
      val obsMapOut = withProgram {
        for {
          _ <- obsMapIn.toList.traverse { case (i,o) => ObservationDao.insert(Observation.Id(pid, i), o) }
          o <- ObservationDao.queryAll(pid)
        } yield o
      }

      obsMapOut shouldEqual obsMapIn
    }
  }

}
