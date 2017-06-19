/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package gem.ocs2

import doobie.imports._
import gem.{Dataset, Log, Observation, Program, Step, User}
import gem.config.{ StaticConfig, DynamicConfig }
import gem.dao.UserDao
import gem.ocs2.Decoders._
import gem.ocs2.pio.PioDecoder

import java.io.File

import scala.xml.{XML, Elem}

import scalaz._
import Scalaz._
import scalaz.effect._


/** Imports OCS2 program files exported to the same format sent by the
  * Ocs3ExportServlet at http://g[ns]odb:8442/ocs3/fetch/programId or
  * by using the OSGi shell command "exportOcs3" from the ODB shell.
  */
object FileImporter extends SafeApp with DoobieClient {

  type Obs  = Observation[StaticConfig, Step[DynamicConfig]]
  type Prog = Program[Obs]

  val dir = new File("archive")

  val checkArchive: IO[Unit] =
    IO(dir.isDirectory).flatMap { b =>
      b.unlessM(IO(sys.error("""
        |
        |** Root of project needs an archive/ dir with program xml files in it.
        |** Try ln -s /path/to/some/stuff archive
        |""".stripMargin)))
    }

  val clean: ConnectionIO[Unit] =
    for {
      _ <- sql"truncate program cascade".update.run
      _ <- sql"truncate log".update.run
      _ <- sql"delete from semester".update.run
    } yield ()

  def read(f: File): IO[Elem] =
    IO(XML.loadFile(f))

  def insert(u: User[_], p: Program[Observation[StaticConfig, Step[DynamicConfig]]], ds: List[Dataset], log: Log[ConnectionIO]): ConnectionIO[Unit] =
    Importer.writeProgram(p, ds)(u, log)

  def readAndInsert(u: User[_], f: File, log: Log[ConnectionIO]): IO[Unit] =
    read(f).flatMap { elem =>
      PioDecoder[(Prog, List[Dataset])].decode(elem) match {
        case -\/(err)     => sys.error(s"Problem parsing ${f.getName}: " + err)
        case \/-((p, ds)) => log.log(u, s"insert ${p.id}")(insert(u, p, ds, log)).transact(xa)
      }
    }.except(e => IO(e.printStackTrace))

  def xmlFiles(num: Int): IO[List[File]] =
    IO(dir.listFiles.toList.filter(_.getName.toLowerCase.endsWith(".xml"))).map(_.take(num))

  def readAndInsertAll(u: User[_], num: Int, log: Log[ConnectionIO]): IO[Unit] =
    xmlFiles(num).flatMap(_.traverse_(readAndInsert(u, _, log)))

  override def runl(args: List[String]): IO[Unit] =
    for {
      u <- UserDao.selectRoot.transact(xa)
      l <- Log.newLog[ConnectionIO]("importer", lxa).transact(xa)
      n <- IO(args.headOption.map(_.toInt).getOrElse(Int.MaxValue))
      _ <- checkArchive
      _ <- IO(configureLogging)
      _ <- clean.transact(xa)
      _ <- readAndInsertAll(u, n, l)
      _ <- l.shutdown(5 * 1000).transact(xa) // if we're not done soon something is wrong
      _ <- IO.putStrLn("Done.")
    } yield ()

}
