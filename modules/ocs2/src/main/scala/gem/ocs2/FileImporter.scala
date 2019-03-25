// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import cats.effect.IO, cats.implicits._
import doobie._, doobie.implicits._
import gem.{ Dataset, Log, Program, User }
import gem.dao.{ DatabaseConfiguration, UserDao }
import gem.ocs2.Decoders._
import gem.ocs2.pio.PioDecoder
import java.io.File
import org.flywaydb.core.Flyway
import scala.xml.{XML, Elem}


/** Imports OCS2 program files exported to the same format sent by the
  * Ocs3ExportServlet at http://g[ns]odb:8442/ocs3/fetch/programId or
  * by using the OSGi shell command "exportOcs3" from the ODB shell.
  */
object FileImporter extends DoobieClient {

  private val conf = DatabaseConfiguration.forTesting
  private val xa   = conf.transactor[IO]

  val dir: File = new File("archive")

  val checkArchive: IO[Unit] =
    IO(dir.isDirectory).flatMap { b =>
      IO(if (b) () else sys.error("""
        |
        |** Root of project needs an archive/ dir with program xml files in it.
        |** Try ln -s /path/to/some/stuff archive
        |""".stripMargin))
    }

  val clean: IO[Int] =
    IO {
      val flyway = Flyway.configure.dataSource(conf.connectUrl, conf.userName, conf.password).load
      flyway.clean()
      flyway.migrate()
    }

  def read(f: File): IO[Elem] =
    IO(XML.loadFile(f))

  def readAndInsert(u: User[_], f: File, log: Log[ConnectionIO]): IO[Unit] =
    read(f).flatMap { elem =>
      PioDecoder[(Program, List[Dataset])].decode(elem) match {
        case Left(err)      => sys.error(s"Problem parsing ${f.getName}: $err")
        case Right((p, ds)) => log.log(u, s"insert ${p.id}")(Importer.importProgram(p, ds)).transact(xa)
      }
    }.handleErrorWith(e => IO(e.printStackTrace))

  def xmlFiles(num: Int): IO[List[File]] =
    IO(dir.listFiles.toList.filter(_.getName.toLowerCase.endsWith(".xml"))).map(_.take(num))

  def readAndInsertAll(u: User[_], num: Int, log: Log[ConnectionIO]): IO[Unit] =
    xmlFiles(num).flatMap(_.traverse_(readAndInsert(u, _, log)))

  def runl(args: List[String]): IO[Unit] =
    for {
      u <- UserDao.selectRootUser.transact(xa)
      l <- Log.newLog[ConnectionIO]("importer", xa).transact(xa)
      n <- IO(args.headOption.map(_.toInt).getOrElse(Int.MaxValue))
      _ <- checkArchive
      _ <- configureLogging[IO]
      _ <- clean
      _ <- readAndInsertAll(u, n, l)
      _ <- l.shutdown(5 * 1000).transact(xa) // if we're not done soon something is wrong
      _ <- IO(Console.println("Done.")) // scalastyle:off console.io
    } yield ()

  def main(args: Array[String]): Unit =
    runl(args.toList).unsafeRunSync

}
