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

import gem.Log
import gem.config.{F2SmartGcalKey, GcalConfig, SmartGcalKey}
import gem.dao.{SmartGcalDao, UserDao}
import gem.enum.{GcalBaselineType, GcalLampType}
import gem.ocs2.pio.PioParse

import java.io.File
import java.time.Duration
import doobie.imports._

import scala.io.Source
import scala.reflect.ClassTag
import scalaz._
import Scalaz._
import scalaz.effect._


/** Importer for SmartGcal CSV files.  Note, these files use display values in
  * some cases instead of the seqexec values since they were meant to be edited
  * by science staff.
  */
object SmartGcalImporter extends SafeApp with DoobieClient {

  implicit class ParseOps(s: String) {
    def parseAs[A](parse: PioParse[A])(implicit ev: ClassTag[A]): A =
      parse(s).getOrElse { sys.error(s"Could not parse '$s' as ${ev.runtimeClass.getName}") }
  }

  // KeyParser accepts a list of String entries, parses the first n entries
  // into a SmartGcalKey and returns it along with the remaining entries.
  type KeyParser = (List[String]) => (SmartGcalKey, List[String])

  /** Instrument specific information needed to parse its smart gcal
    * configuration.
    *
    * @param tableName name of table where the data is written
    * @param filePrefix file name prefix, which is extended with "_ARC.csv" and
    *                   "_FLAT.csv" to create the full file names.
    * @param parser reads the first part of a line of entries into a
    *               SmartGcalKey appropriate for the instrument
    */
  final case class SmartDef(tableName: String, filePrefix: String, parser: KeyParser)

  // -------------------------------------------------------------------------
  // Add your smartgcal tables here.
  // -------------------------------------------------------------------------
  val smartTables = List(
    SmartDef("smart_f2", "Flamingos2", parseF2)
  )

  def parseF2(input: List[String]): (SmartGcalKey, List[String]) = {
    import Parsers.Flamingos2._

    val disperserS :: filterS :: fpuS :: gcal = input

    val d = disperserS.parseAs(disperserDisplayValue)
    val f = filterS   .parseAs(filterDisplayValue   )
    val u = fpuS      .parseAs(fpuDisplayValue      )

    (F2SmartGcalKey(d, f, u), gcal)
  }

  // -------------------------------------------------------------------------
  // Implementation Details
  // -------------------------------------------------------------------------

  val dir = new File("smartgcal")

  val checkSmartDir: IO[Unit] =
    IO(dir.isDirectory).flatMap { b =>
      b.unlessM(IO(sys.error(
        """
          |** Root of project needs a "smartgcal/" dir with smart gcal config files in it.
          |** Try ln -s /path/to/some/smart/gcal smartgcal
          |** (for example ~/.ocs15/Gemini\ OT\ 2017A.1.1.1_mac/data/jsky.app.ot/smartgcal)
        """.stripMargin)))
    }

  /** Truncates all the smart gcal tables. */
  val clean: ConnectionIO[Unit] =
    for {
      _ <- smartTables.map(_.tableName).traverseU(t => Update0(s"TRUNCATE $t CASCADE", None).run)
      _ <- sql"DELETE FROM gcal WHERE step_id IS NULL".update.run
    } yield ()

  /** Reads a smart gcal csv file into a List of lines where each line is a
    * list of String entries.  This relies on the fact that all the smart gcal
    * configuration files have the same format.  A header line with a timestamp,
    * a header line describing the fields, and then the csv data.  Comments are
    * interspersed but always start the line with a #.  Various empty columns
    * separate the instrument specific part from the common gcal config part in
    * each line of input.
    */
  def readSmartGcalFile(f: File): IO[List[List[String]]] = {
    def toLines(f: File): IO[List[String]] =
      IO {
        val src = Source.fromFile(f, "UTF-8")
        try { src.getLines.toList } finally { src.close }
      }

    toLines(f).map { lines =>
      lines.filterNot(_.startsWith("#")).drop(2).map { line =>
        line.split(',').filterNot(_.isEmpty).toList
      }
    }
  }

  def parseGcal(input: List[String]): (GcalBaselineType, GcalConfig) = {
    import Parsers.Calibration._

    val _ :: filterS :: diffuserS :: lampS :: shutterS :: expS :: coaddsS :: baselineS :: Nil = input

    val l = lampS    .parseAs(lamp    )
    val f = filterS  .parseAs(filter  )
    val d = diffuserS.parseAs(diffuser)
    val s = shutterS .parseAs(shutter )
    val e = Duration.ofMillis(expS.toLong * 1000)
    val c = coaddsS.toShort

    val b = GcalBaselineType.unsafeFromTag(baselineS)

    (b, GcalConfig(l, f, d, s, e, c))
  }

  def parseFile(input: List[String], l: GcalLampType, parser: KeyParser): (GcalLampType, GcalBaselineType, SmartGcalKey, GcalConfig) = {
    val (k, r) = parser(input)
    val (b, g) = parseGcal(r)
    (l, b, k, g)
  }

  type SmartGcalLine = (GcalLampType, GcalBaselineType, SmartGcalKey, GcalConfig)

  /** A program that will read all SmartGcal config .csv files into a list of
    * parsed lines.
    */
  val readConfig: IO[List[SmartGcalLine]] = {
    // Creates a program that will read a single .csv file into a parsed list of
    // smart gcal configuration.  There is a file per FLAT/ARC per instrument.
    def readCsv(fileNamePrefix: String, lampType: GcalLampType, parser: KeyParser): IO[List[SmartGcalLine]] =
      readSmartGcalFile(new File(dir, s"${fileNamePrefix}_${lampType.tag.toUpperCase}.csv")).map {
        _.map(parseFile(_, lampType, parser))
      }

    // Creates a program that will reads the FLAT/ARC files for a single
    // instrument with the given file name prefix.
    def readInstrument(fileNamePrefix: String, parser: KeyParser): IO[List[SmartGcalLine]] =
      GcalLampType.all.traverseU(t => readCsv(fileNamePrefix, t, parser)).map(_.flatten)

    // A program that will read all smart gcal config files for all instruments
    smartTables.traverseU(d => readInstrument(d.filePrefix, d.parser)).map(_.flatten)
  }

  /** Creates a program that will clean all smart gcal config information in
    * the database and then import the given configuration.
    */
  def cleanAndImport(lines: List[SmartGcalLine]): IO[Unit] =
    (clean *> lines.traverseU { case (l, b, k, g) =>
      SmartGcalDao.insert(l, b, k, g)
    }).void.transact(xa)

  override def runl(args: List[String]): IO[Unit] =
    for {
      u <- UserDao.selectRoot.transact(xa)
      l <- Log.newLog[IO]("smartgcal importer", lxa)
      _ <- checkSmartDir
      _ <- IO(configureLogging)
      c <- readConfig
      _ <- cleanAndImport(c)
      _ <- l.shutdown(5 * 1000)
      _ <- IO.putStrLn("Done.")
    } yield ()
}
