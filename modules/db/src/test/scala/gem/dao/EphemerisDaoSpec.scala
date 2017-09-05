// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.arb.ArbEphemeris._
import gem.arb.ArbEphemerisKey._
import gem.arb.ArbTime._
import gem.math.Ephemeris
import gem.util.InstantMicros

import cats.implicits._
import doobie._
import doobie.implicits._

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.NonUnitStatements"))
class EphemerisDaoSpec extends PropSpec with PropertyChecks with DaoTest {

  type EphemerisMap = Map[EphemerisKey, Ephemeris]

  //
  // Inserts all the ephemeris data and executes the given command.
  //
  private def execTest[A](m: EphemerisMap, ca: ConnectionIO[A]): A =
    (m.toList.traverse((EphemerisDao.insert _).tupled) *> ca)
      .transact(xa)
      .unsafeRunSync

  property("EphemerisDao should return empty ephmeris if the key is not found") {
    forAll { (k: EphemerisKey, m: EphemerisMap) =>
      val e = execTest(m - k, EphemerisDao.selectAll(k))
      e shouldEqual Ephemeris.Empty
    }
  }

  property("EphemerisDao should selectAll") {
    forAll { (k: EphemerisKey, e: Ephemeris, m: EphemerisMap) =>
      val eʹ = execTest(m + (k -> e), EphemerisDao.selectAll(k))
      e shouldEqual eʹ
    }
  }

  property("EphemerisDao should selectRange") {
    forAll { (k: EphemerisKey, e: Ephemeris, m: EphemerisMap, i0: InstantMicros, i1: InstantMicros) =>
      val List(start, end) = List(i0, i1).sorted
      val eʹ = execTest(m + (k -> e), EphemerisDao.selectRange(k, start, end))
      e.toMap.range(start, end) shouldEqual eʹ.toMap
    }
  }

  property("EphemerisDao should delete by key") {
    forAll { (k: EphemerisKey, e: Ephemeris, m: EphemerisMap) =>
      val eʹ = execTest(m + (k -> e), EphemerisDao.delete(k) *> EphemerisDao.selectAll(k))
      eʹ shouldEqual Ephemeris.Empty
    }
  }

  property("EphemerisDao should not delete others") {
    forAll { (k: EphemerisKey, e: Ephemeris, m: EphemerisMap) =>
      val p  = EphemerisDao.delete(k) *>
                 (m - k).keys.toList.traverse(kʹ => EphemerisDao.selectAll(kʹ).tupleLeft(kʹ)).map(_.toMap)

      (m - k) shouldEqual execTest(m + (k -> e), p)
    }
  }

  property("EphemerisDao update should replace existing") {
    forAll { (k: EphemerisKey, e0: Ephemeris, e1: Ephemeris, m: EphemerisMap) =>
      val p = EphemerisDao.update(k, e1) *>
                EphemerisDao.selectAll(k)

      e1 shouldEqual execTest(m + (k -> e0), p)
    }
  }

  property("EphemerisDao should generate UserSupplied ephemeris keys") {

    // Select a handful of ids and make sure they are unique.
    //
    // NOTE: even though we abort the transaction and rollback, this doesn't
    // reset the sequence id counter because SEQUENCE is "non-transactional".
    // A flywayClean followed by flywayMigrate will reset the sequence of
    // course, or an explicit SELECT setval('user_ephemeris_id', 0, false).

    val ids: List[EphemerisKey.UserSupplied] =
      (1 to 10)
        .toList
        .traverse(_ => EphemerisDao.nextUserSuppliedKey)
        .transact(xa)
        .unsafeRunSync

    ids.distinct shouldEqual ids
  }
}
