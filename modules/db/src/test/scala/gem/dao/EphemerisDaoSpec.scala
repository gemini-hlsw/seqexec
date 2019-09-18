// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.arb.ArbEnumerated._
import gem.arb.ArbEphemeris._
import gem.arb.ArbEphemerisKey._
import gem.arb.ArbEphemerisMeta._
import gem.enum.Site
import gem.math.Ephemeris
import gem.util.Timestamp
import gem.arb.ArbTimestamp._

import cats.implicits._
import cats.tests.CatsSuite

import doobie._
import doobie.implicits._

import fs2.Stream

import org.scalacheck._
import org.scalacheck.Arbitrary._

class EphemerisDaoSpec extends CatsSuite with DaoTest {

  import EphemerisDaoSpec._

  //
  // Inserts all the ephemeris data and executes the given command.
  //
  private def execTest[A](m: EphemerisMap, ca: ConnectionIO[A]): A =
    ((m.toList.traverse { case (ks, e) => EphemerisDao.insert(ks.key, ks.site, e) }) *> ca)
      .transact(xa)
      .unsafeRunSync

  test("EphemerisDao should return empty ephmeris if the key is not found") {
    forAll { (ks: KS, m: EphemerisMap) =>
      val e = execTest(m - ks, EphemerisDao.selectAll(ks.key, ks.site))
      e shouldEqual Ephemeris.empty
    }
  }

  test("EphemerisDao should selectAll") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>
      val eʹ = execTest(m + (ks -> e), EphemerisDao.selectAll(ks.key, ks.site))
      e shouldEqual eʹ
    }
  }

  test("EphemerisDao should selectRange") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap, i0: Timestamp, i1: Timestamp) =>
      val List(start, end) = List(i0, i1).sorted
      val eʹ = execTest(m + (ks -> e), EphemerisDao.selectRange(ks.key, ks.site, start, end))
      e.toMap.range(start, end) shouldEqual eʹ.toMap
    }
  }

  test("EphemerisDao should delete by key") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>
      val eʹ = execTest(m + (ks -> e), EphemerisDao.delete(ks.key, ks.site) *> EphemerisDao.selectAll(ks.key, ks.site))
      eʹ shouldEqual Ephemeris.empty
    }
  }

  test("EphemerisDao should not delete others") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>
      val p  = EphemerisDao.delete(ks.key, ks.site) *>
                 (m - ks).keys.toList.traverse(ksʹ => EphemerisDao.selectAll(ksʹ.key, ksʹ.site).tupleLeft(ksʹ)).map(_.toMap)

      (m - ks) shouldEqual execTest(m + (ks -> e), p)
    }
  }

  test("EphemerisDao update should replace existing") {
    forAll { (ks: KS, e0: Ephemeris, e1: Ephemeris, m: EphemerisMap) =>
      val p = EphemerisDao.update(ks.key, ks.site, e1) *>
                EphemerisDao.selectAll(ks.key, ks.site)

      e1 shouldEqual execTest(m + (ks -> e0), p)
    }
  }

  test("EphemerisDao should generate UserSupplied ephemeris keys") {

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

  test("EphemerisDao meta select should return None if unknown key") {
    forAll { (ks: KS) =>
      val meta = EphemerisDao.selectMeta(ks.key, ks.site)
        .transact(xa)
        .unsafeRunSync

      meta shouldEqual None
    }
  }

  test("EphemerisDao meta should roundtrip") {
    forAll { (ks: KS, m: EphemerisMeta) =>
      val p = EphemerisDao.insertMeta(ks.key, ks.site, m) *>
                EphemerisDao.selectMeta(ks.key, ks.site)

      p.transact(xa).unsafeRunSync shouldEqual Some(m)
    }
  }

  test("EphemerisDao meta should select by key and site") {
    forAll { (head: (KS, EphemerisMeta), tail: List[(KS, EphemerisMeta)], i: Int) =>

      val env = EphemerisMetaTestEnv(head, tail, i)

      // Select from the DB the one that we picked
      val selectOne = EphemerisDao.selectMeta(env.key, env.site)

      // Run the test
      val res = (env.insertAll *> selectOne).transact(xa).unsafeRunSync

      res shouldEqual Some(env.meta)
    }
  }

  test("EphemerisDao meta should update") {
    forAll { (head: (KS, EphemerisMeta), tail: List[(KS, EphemerisMeta)], i: Int, meta: EphemerisMeta) =>

      val env = EphemerisMetaTestEnv(head, tail, i)

      // Update the one that we picked
      val updateOne = EphemerisDao.updateMeta(env.key, env.site, meta)

      // Try to select the updated one
      val selectOne = EphemerisDao.selectMeta(env.key, env.site)

      // Run the test
      val res = (env.insertAll *> updateOne *> selectOne).transact(xa).unsafeRunSync

      res shouldEqual Some(meta)
    }
  }

  test("EphemerisDao meta should delete") {
    forAll { (head: (KS, EphemerisMeta), tail: List[(KS, EphemerisMeta)], i: Int) =>

      val env = EphemerisMetaTestEnv(head, tail, i)

      // Delete the one that we picked
      val deleteOne = EphemerisDao.deleteMeta(env.key, env.site)

      // Try to select the deleted one
      val selectOne = EphemerisDao.selectMeta(env.key, env.site)

      // Run the test
      val res = (env.insertAll *> deleteOne *> selectOne).transact(xa).unsafeRunSync

      res shouldEqual None
    }
  }

  test("EphemerisDao should stream insert") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>

      val mʹ = m + (ks -> e)
      val p  = (mʹ.toList.traverse { case (ks, e) =>
        Stream.emits(e.toMap.toList).covary[ConnectionIO]
          .through(EphemerisDao.streamInsert(ks.key, ks.site))
          .compile
          .drain
      }) *> EphemerisDao.selectAll(ks.key, ks.site)

      e shouldEqual p.transact(xa).unsafeRunSync
    }
  }

  test("EphemerisDao should stream update") {
    forAll { (ks: KS, e0: Ephemeris, e1: Ephemeris, m: EphemerisMap) =>

      val mʹ = m + (ks -> e0)

      // Setup a program that will insert all the ephemeris maps.
      val p  = mʹ.toList.traverse { case (ks, e) =>
        Stream.emits(e.toMap.toList).covary[ConnectionIO]
          .through(EphemerisDao.streamInsert(ks.key, ks.site))
          .compile
          .drain
      }

      // Now stream update the ephemeris elements in e0 to those in e1
      val pʹ = p *> Stream.emits(e1.toMap.toList).covary[ConnectionIO]
                      .through(EphemerisDao.streamUpdate(ks.key, ks.site))
                      .compile
                      .drain

      // Select the values with the matching key and site, which should now be
      // updated
      val pʹʹ = pʹ *> EphemerisDao.selectAll(ks.key, ks.site)

      e1 shouldEqual pʹʹ.transact(xa).unsafeRunSync
    }
  }

  test("EphemerisDao should select times") {
    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>

      val p  = EphemerisDao.selectTimes(ks.key, ks.site)
      val em = e.toMap

      val expected =
        if (em.isEmpty) Option.empty[(Timestamp, Timestamp)]
        else Some((em.firstKey, em.lastKey))

      expected shouldEqual execTest(m + (ks -> e), p)
    }
  }

  test("EphemerisDao bracketRange") {
    import Timestamp.{ Max, Min }

    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>
      val em = e.toMap

      val (eMin, qMin) = (for {
        e <- em.headOption.map(_._1)
        q <- e.plusMicros(1L)
      } yield (e, q)).getOrElse((Min, Min))

      val (eMax, qMax) = (for {
        e <- em.lastOption.map(_._1)
        q <- e.plusMicros(-1L)
      } yield (e, q)).getOrElse((Max, Max))

      val p = EphemerisDao.bracketRange(ks.key, ks.site, qMin, qMax)

      (eMin, eMax) shouldEqual execTest(m + (ks -> e), p)
    }
  }

  test("EphemerisDao bracketRange exact") {
    import Timestamp.{ Max, Min }

    forAll { (ks: KS, e: Ephemeris, m: EphemerisMap) =>
      val em = e.toMap

      val (min, max) = if (em.isEmpty) (Min, Max) else (em.firstKey, em.lastKey)

      val p = EphemerisDao.bracketRange(ks.key, ks.site, min, max)

      (min, max) shouldEqual execTest(m + (ks -> e), p)
    }
  }

  test("EphemerisDao should select None times if no matching ephemeris") {
    forAll { (ks: KS, m: EphemerisMap) =>

      val p  = EphemerisDao.selectTimes(ks.key, ks.site)

      None shouldEqual execTest(m - ks, p)
    }

  }

  test("EphemerisDao should select all keys") {
    forAll { (head: (KS, EphemerisMeta), tail: List[(KS, EphemerisMeta)], i: Int) =>

      val env = EphemerisMetaTestEnv(head, tail, i)

      // The keys we expect to select from the database.
      val expectedKeys = (head :: tail).collect {
        case (KS(k, s), _) if s == env.site => k
      }.toSet

      val selectKeys = EphemerisDao.selectKeys(env.site)

      // Run the test
      val res = (env.insertAll *> selectKeys).transact(xa).unsafeRunSync

      res shouldEqual expectedKeys
    }
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

  /** Environment for running EphemerisMeta tests.  It is a program for
    * inserting a collection of ephemeris meta for testing and the key, site,
    * and EphemerisMeta corresponding to one of the members of the collection
    * at random.
    *
    * @param insertAll action that inserts an initial collection of ephemeris
    *                  meta values
    * @param key       a random key referring to one of the values that will be
    *                  inserted
    * @param site      a random site referring to one of the values that will be
    *                  inserted
    * @param meta      ephemeris meta value corresponding to (key, site)
    */
  final class EphemerisMetaTestEnv(
    val insertAll: ConnectionIO[Unit],
    val key:       EphemerisKey,
    val site:      Site,
    val meta:      EphemerisMeta)

  object EphemerisMetaTestEnv {
    def apply(head: (KS, EphemerisMeta),
              tail: List[(KS, EphemerisMeta)],
              i:    Int): EphemerisMetaTestEnv = {

      // Eliminate duplicate keys
      val all = (head :: tail).toMap.toList

      // Pick an element at random
      val index = if (i == Int.MinValue) i + 1 else i
      val (ks, em) = all(index.abs % all.size)

      // Insert all the ephemeris meta data
      val insertAll = all.traverse { case (ksʹ, emʹ) =>
        EphemerisDao.insertMeta(ksʹ.key, ksʹ.site, emʹ)
      }.void

      new EphemerisMetaTestEnv(insertAll, ks.key, ks.site, em)
    }
  }
}
