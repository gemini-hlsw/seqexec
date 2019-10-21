// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import cats.implicits._
import doobie._, doobie.implicits._
import doobie.postgres.implicits._
import gem._
import gem.enum.ProgramRole
import gsp.math.syntax.prism._
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.TreeMap


class UserDaoSpec extends AnyFlatSpec with Matchers with DaoTest {

  private val pid1 = Program.Id.fromString.unsafeGet("GS-1234A-Q-1")
  private val pid2 = Program.Id.fromString.unsafeGet("GS-1234A-Q-3")

  private val prog1 = Program(pid1, "prog1", TreeMap.empty)
  private val prog2 = Program(pid2, "prog2", TreeMap.empty)

  private val user1 =
    User[ProgramRole]("bob", "Bob", "Dobbs", "bob@dobbs.com", false,
      Map(
        pid1 -> Set(ProgramRole.PI, ProgramRole.GEM),
        pid2 -> Set(ProgramRole.PI)
      )
    )

  private val user2 =
    User[ProgramRole]("homer", "Homer", "Simpson", "chunkylover53@aol.com", false, Map.empty)

  "UserDao" should "select the root user" in {
    val r = UserDao.selectRootUser.transact(xa).unsafeRunSync
    r.id shouldEqual User.Id.Root
  }

  it should "round-trip a user with roles" in {
    val prog: ConnectionIO[Option[User[ProgramRole]]] =
      for {
        _ <- ProgramDao.insert(prog1)
        _ <- ProgramDao.insert(prog2)
        _ <- UserDao.insertUser(user1, "pass")
        u <- UserDao.selectUserʹ(user1.id, "pass")
      } yield u
    prog.transact(xa).unsafeRunSync shouldEqual Some(user1)
  }

  it should "set roles" in {
    val prog: ConnectionIO[Option[User[ProgramRole]]] =
      for {
        _ <- ProgramDao.insert(prog1)
        _ <- UserDao.insertUser(user2, "pass")
        _ <- UserDao.setRole(user2.id, prog1.id, ProgramRole.PI)
        _ <- UserDao.setRole(user2.id, prog1.id, ProgramRole.GEM)
        u <- UserDao.selectUserʹ(user2.id, "pass")
      } yield u
    prog.transact(xa).unsafeRunSync.flatMap(_.roles.get(prog1.id))
      .shouldEqual(Some(Set(ProgramRole.PI, ProgramRole.GEM)))
  }

  it should "unset roles" in {
    val prog: ConnectionIO[Option[User[ProgramRole]]] =
      for {
        _ <- ProgramDao.insert(prog1)
        _ <- ProgramDao.insert(prog2)
        _ <- UserDao.insertUser(user1, "pass")
        _ <- UserDao.unsetRole(user1.id, prog1.id, ProgramRole.GEM)
        u <- UserDao.selectUserʹ(user1.id, "pass")
      } yield u
    prog.transact(xa).unsafeRunSync.flatMap(_.roles.get(prog1.id))
      .shouldEqual(Some(Set(ProgramRole.PI)))
  }

  it should "fail to select a user with an incorrect password" in {
    val prog: ConnectionIO[Option[User[ProgramRole]]] =
      for {
        _ <- UserDao.insertUser(user2, "pass")
        u <- UserDao.selectUserʹ(user2.id, "banana")
      } yield u
    prog.transact(xa).unsafeRunSync shouldEqual None
  }

  it should "change password if correct original password is specified" in {
    val prog: ConnectionIO[Boolean] =
      for {
        _ <- UserDao.insertUser(user2, "pass")
        b <- UserDao.changePassword(user2.id, "pass", "eskimo")
      } yield b
    prog.transact(xa).unsafeRunSync shouldEqual true
  }

  it should "fail to change password if incorrect original password is specified" in {
    val prog: ConnectionIO[Boolean] =
      for {
        _ <- UserDao.insertUser(user2, "pass")
        b <- UserDao.changePassword(user2.id, "banana", "eskimo")
      } yield b
    prog.transact(xa).unsafeRunSync shouldEqual false
  }

  it should "raise a key violation on duplicate id" in {
    val prog: ConnectionIO[Boolean] =
      for {
        _ <- UserDao.insertUser(user2, "pass1")
        _ <- UserDao.insertUser(user2, "pass2")
      } yield true
    prog.onUniqueViolation(false.pure[ConnectionIO]).transact(xa).unsafeRunSync shouldEqual false
  }

}
