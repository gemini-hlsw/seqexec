// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.sql

import doobie.imports._
import gem.sql.enum._
import java.nio.file._
import scalaz._, Scalaz._
import scalaz.effect._

object Main extends SafeApp {

  val xa: Transactor[IO] =
    Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:gem",
      "postgres",
      ""
    )

  val enums: ConnectionIO[List[EnumDef]] = {
    List(
      F2Enums  .enums,
      GcalEnums.enums,
      GmosEnums.enums,
      MiscEnums.enums
    ).join.sequence
  }

  def write(dir: Path, d: EnumDef): IO[Unit] = {
    val out = dir.resolve(d.fileName)
    IO(Files.write(out, d.text.getBytes("UTF-8"))) *>
    IO.putStrLn(s"Wrote $out")
  }

  def writeAll(dir: Path): IO[Unit] =
    for {
      eds <- enums.transact(xa)
      _   <- IO(Files.createDirectories(dir))
      _   <- eds.traverse_(write(dir, _))
    } yield ()

  override def runl(args: List[String]): IO[Unit] =
    args match {
      case List(dirName) => IO(Paths.get(dirName).toAbsolutePath).flatMap(writeAll)
      case _             => IO.putStrLn("usage: <main> path/to/output/dir")
    }

}
