// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.arb.ArbEnumerated._
import gem.arb.ArbEphemeris._
import gem.arb.ArbEphemerisKey._
import gem.arb.ArbTime._
import gem.enum.Site
import gem.math.Ephemeris
import gem.util.InstantMicros

import cats.implicits._
import doobie._
import doobie.implicits._

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.NonUnitStatements"))
class EphemerisDaoSpec extends PropSpec with PropertyChecks with DaoTest {

  import EphemerisDaoSpec._

  //
  // Inserts all the ephemeris data and executes the given command.
  //
  private def execTest[A](m: EphemerisMap, ca: ConnectionIO[A]): A =
    ((m.toList.traverse { case (ks, e) => EphemerisDao.insert(ks.key, ks.site, e) }) *> ca)
      .transact(xa)
      .unsafeRunSync

  property("EphemerisDao should return empty ephmeris if the key is not found") {
    forAll { (ks: KS, m: EphemerisMap) =>
      val e = execTest(m - ks, EphemerisDao.selectAll(ks.key, ks.site))
      e shouldEqual Ephemeris.Empty
    }
  }

  property("EphemerisDao should selectAll") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>
      val eʹ = execTest(m + (ks -> e), EphemerisDao.selectAll(ks.key, ks.site))
      e shouldEqual eʹ
    }
  }

  property("EphemerisDao should selectRange") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap, i0: InstantMicros, i1: InstantMicros) =>
      val List(start, end) = List(i0, i1).sorted
      val eʹ = execTest(m + (ks -> e), EphemerisDao.selectRange(ks.key, ks.site, start, end))
      e.toMap.range(start, end) shouldEqual eʹ.toMap
    }
  }

  property("EphemerisDao should delete by key") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>
      val eʹ = execTest(m + (ks -> e), EphemerisDao.delete(ks.key, ks.site) *> EphemerisDao.selectAll(ks.key, ks.site))
      eʹ shouldEqual Ephemeris.Empty
    }
  }

  property("EphemerisDao should not delete others") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>
      val p  = EphemerisDao.delete(ks.key, ks.site) *>
                 (m - ks).keys.toList.traverse(ksʹ => EphemerisDao.selectAll(ksʹ.key, ksʹ.site).tupleLeft(ksʹ)).map(_.toMap)

      (m - ks) shouldEqual execTest(m + (ks -> e), p)
    }
  }

  property("EphemerisDao update should replace existing") {
    forAll { (ks: KS, e0: Ephemeris, e1: Ephemeris, m: EphemerisMap) =>
      val p = EphemerisDao.update(ks.key, ks.site, e1) *>
                EphemerisDao.selectAll(ks.key, ks.site)

      e1 shouldEqual execTest(m + (ks -> e0), p)
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

object EphemerisDaoSpec {
  final case class KS(key: EphemerisKey, site: Site)

  implicit val arbitraryKS: Arbitrary[KS] =
    Arbitrary {
      for {
        k <- arbitrary[EphemerisKey]
        s <- arbitrary[Site]
      } yield KS(k, s)
    }

  type EphemerisMap = Map[KS, Ephemeris]
}
