// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.implicits._
import doobie.implicits._
import gem.Observation
import gem.enum._
import gem.math.Index
import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._

class ObservationDaoSpec extends PropSpec with PropertyChecks with DaoTest {

  property("ObservationDao should select all observation ids for a program") {
    forAll(genObservationMap(limit = 50)) { obsMap =>
      val oids = withProgram {
        for {
          _ <- obsMap.toList.traverse { case (i,o) => ObservationDao.insert(Observation.Id(pid, i), o) }
          o <- ObservationDao.selectIds(pid)
        } yield o
      }

      oids.toSet shouldEqual obsMap.keys.map(idx => Observation.Id(pid, idx)).toSet
    }
  }

  property("ObservationDao should select flat observations") {
    val oid = Observation.Id(pid, Index.One)

    forAll { (obsIn: Observation.Full) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(oid, obsIn)
          o <- ObservationDao.selectFlat(oid)
        } yield o
      }

      // Take the generated observation, remove the targets and steps, and map
      // the the static config to the instrument.
      val expected = Observation.staticConfigFunctor.map(
                       Observation.targetsFunctor.map(obsIn)(_.asterism.map(AsterismType.of))
                     )(_.instrument).copy(steps = Nil)

      obsOut shouldEqual expected // obsIn.leftMap(_.instrument).copy(steps = Nil)
    }
  }

  property("ObservationDao should select static-only observations") {
    val oid = Observation.Id(pid, Index.One)

    forAll { (obsIn: Observation.Full) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(oid, obsIn)
          o <- ObservationDao.selectStatic(oid)
        } yield o
      }

      // Take the generated observation and remove the targets and steps
      val expected = Observation.targetsFunctor
                       .map(obsIn)(_.asterism.map(AsterismType.of))
                       .copy(steps = Nil)

      obsOut shouldEqual expected
    }
  }

  property("ObservationDao should select target-only observations") {
    val oid = Observation.Id(pid, Index.One)

    forAll { (obsIn: Observation.Full) =>
      val obsOut = withProgram {
        for {
          _ <- ObservationDao.insert(oid, obsIn)
          o <- ObservationDao.selectTargets(oid)
        } yield o
      }

      // Take the generated observation, replace the static config with the
      // instrument type and remove the steps.
      val expected = Observation.staticConfigFunctor
                       .map(obsIn)(_.instrument)
                       .copy(steps = Nil)

      obsOut shouldEqual expected
    }
  }

  property("ObservationDao should roundtrip complete observations") {
    val oid = Observation.Id(pid, Index.One)

    forAll { (obsIn: Observation.Full) =>
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
    forAll(genObservationMap(limit = 50)) { obsMapIn =>
      val obsMapOut = withProgram {
        for {
          _ <- obsMapIn.toList.traverse { case (i,o) => ObservationDao.insert(Observation.Id(pid, i), o) }
          o <- ObservationDao.selectAll(pid)
        } yield o
      }

      obsMapOut shouldEqual obsMapIn
    }
  }

}
