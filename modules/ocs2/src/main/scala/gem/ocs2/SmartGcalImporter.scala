// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import cats.effect.IO, cats.implicits._
import fs2.Stream
import fs2.{io, text}
import gem.Log
import gem.config.GcalConfig
import gem.config.DynamicConfig.SmartGcalKey
import gem.dao.{ SmartGcalDao, UserDao }
import gem.enum.{ GcalBaselineType, GcalLampType }
import gem.ocs2.pio.PioParse
import gem.math.Wavelength

import java.io.File
import java.time.Duration
import doobie._, doobie.implicits._

import scala.reflect.runtime.universe._


/** Importer for SmartGcal CSV files.  Note, these files use display values in
  * some cases instead of the seqexec values since they were meant to be edited
  * by science staff.
  */
object SmartGcalImporter extends DoobieClient {

  implicit class ParseOps(s: String) {
    def parseAs[A: TypeTag](parse: PioParse[A]): A =
      parse(s).getOrElse {
        sys.error(s"Could not parse '$s' as ${typeOf[A]}")
      }
  }

  // KeyParser accepts a list of String entries, parses the first n entries
  // into a SmartGcalDefinitionKey and returns it along with the remaining
  // entries.
  type KeyParser[K]       = (List[String]) => (K, List[String])
  type SmartGcalLine[K]   = (GcalLampType, GcalBaselineType, K, GcalConfig)
  type SmartGcalWriter[K] = (Vector[SmartGcalLine[K]]) => Stream[ConnectionIO, Int]

  // --------------------------------------------------------------------------
  // Add your instruments here.
  // --------------------------------------------------------------------------

  def importAllInst: IO[Unit] = {
    import SmartGcalDao._

    for {
      _ <- importInst("Flamingos2", parseF2,        dropIndexF2,        bulkInsertF2,        createIndexF2       )
      _ <- importInst("GMOS-N",     parseGmosNorth, dropIndexGmosNorth, bulkInsertGmosNorth, createIndexGmosNorth)
      _ <- importInst("GMOS-S",     parseGmosSouth, dropIndexGmosSouth, bulkInsertGmosSouth, createIndexGmosSouth)
    } yield ()
  }

  def parseF2(input: List[String]): (SmartGcalKey.F2, List[String]) = {
    import Parsers.Flamingos2._

    val disperserS :: filterS :: fpuS :: gcal = input

    val d = disperserS.parseAs(disperser)
    val f = filterS   .parseAs(filter   )
    val u = fpuS      .parseAs(fpu      ).flatMap(_.toBuiltin)

    (SmartGcalKey.F2(d, f, u), gcal)
  }

  def parseGmosNorth(input: List[String]): (SmartGcalKey.GmosNorthDefinition, List[String]) = {
    import Parsers.Gmos._
    import Parsers.GmosNorth._

    val disperserS :: filterS :: fpuS :: xBinS :: yBinS :: _ :: ampGainS :: wavelengthMinS :: wavelengthMaxS :: gcal = input

    val d = disperserS.parseAs(disperser)
    val f = filterS   .parseAs(filter   )
    val u = fpuS      .parseAs(fpu)
    val x = xBinS     .parseAs(xBinning)
    val y = yBinS     .parseAs(yBinning)
    val a = ampGainS  .parseAs(ampGain)

    val c = SmartGcalKey.GmosCommon(d, f, u, x, y, a)

    val wmin = wavelengthMinS.parseAs(Parsers.angstroms)
    val wmax = parseMaxWavelength(wavelengthMaxS)

    (SmartGcalKey.GmosDefinition(c, (wmin, wmax)), gcal)
  }

  def parseGmosSouth(input: List[String]): (SmartGcalKey.GmosSouthDefinition, List[String]) = {
    import Parsers.Gmos._
    import Parsers.GmosSouth._

    val disperserS :: filterS :: fpuS :: xBinS :: yBinS :: _ :: ampGainS :: wavelengthMinS :: wavelengthMaxS :: gcal = input

    val d = disperserS.parseAs(disperser)
    val f = filterS   .parseAs(filter   )
    val u = fpuS      .parseAs(fpu)
    val x = xBinS     .parseAs(xBinning)
    val y = yBinS     .parseAs(yBinning)
    val a = ampGainS  .parseAs(ampGain)

    val c = SmartGcalKey.GmosCommon(d, f, u, x, y, a)

    val wmin = wavelengthMinS.parseAs(Parsers.angstroms)
    val wmax = parseMaxWavelength(wavelengthMaxS)

    (SmartGcalKey.GmosDefinition(c, (wmin, wmax)), gcal)
  }

  // -------------------------------------------------------------------------
  // Implementation Details
  // -------------------------------------------------------------------------

  // Where to look for smart gcal csv files.
  val dir: File = new File("smartgcal")

  /** Obtains the file name to use for the given instrument name and lamp type.
    *
    * @param prefix file name prefix, which is extended with "_ARC.csv" and
    *               "_FLAT.csv" to create the full file names.
    * @param lampType flat or arc
    * @return corresponding .csv file
    */
  def fileName(prefix: String, lampType: GcalLampType): String =
    s"${prefix}_${lampType.tag.toUpperCase}.csv"

  val checkSmartDir: IO[Unit] =
    IO(dir.isDirectory).flatMap { b =>
      IO(if (b) () else sys.error(
        """
          |** Root of project needs a "smartgcal/" dir with smart gcal config files in it.
          |** Try ln -s /path/to/some/smart/gcal smartgcal
          |** (for example ~/.ocs15/Gemini\ OT\ 2017A.1.1.1_mac/data/jsky.app.ot/smartgcal)
        """.stripMargin))
    }

  /** Truncates all the smart gcal tables. */
  val clean: ConnectionIO[Unit] =
    sql"TRUNCATE gcal CASCADE".update.run.void

  def parseMaxWavelength(s: String): Wavelength =
    s match {
      case "MAX" => Wavelength.unsafeFromAngstroms(Int.MaxValue)
      case _     => s.parseAs(Parsers.angstroms)
    }

  def parseGcal(input: List[String]): (GcalBaselineType, GcalConfig) = {
    import Parsers.Calibration._

    val _ :: filterS :: diffuserS :: lampS :: shutterS :: expS :: coaddsS :: baselineS :: Nil = input

    val l = lampS.replaceAll(";", ",").parseAs(lamp)
    val f = filterS  .parseAs(filter  )
    val d = diffuserS.parseAs(diffuser)
    val s = shutterS .parseAs(shutter )
    val e = Duration.ofMillis(expS.parseAs(PioParse.long))
    val c = coaddsS  .parseAs(PioParse.short)

    val b = baselineS.parseAs(baseline)

    (b, GcalConfig(l, f, d, s, e, c))
  }


  def parseLine[K](input: List[String], l: GcalLampType, parser: KeyParser[K]): SmartGcalLine[K] = {
    val (k, r) = parser(input)
    val (b, g) = parseGcal(r)
    (l, b, k, g)
  }

  def importInst[K](instFilePrefix: String,
                    parser:         KeyParser[K],
                    unindexer:      ConnectionIO[Int],
                    writer:         SmartGcalWriter[K],
                    indexer:        ConnectionIO[Int]): IO[Unit] = {

    def lines(l: GcalLampType): Stream[IO, SmartGcalLine[K]] =
      io.file.readAll[IO](new File(dir, fileName(instFilePrefix, l)).toPath, 4096)
          .through(text.utf8Decode)
          .through(text.lines)
          .filter(_.trim.nonEmpty)
          .map(_.split(',').map(_.trim).toList)
          .map(parseLine(_, l, parser))

    val prog = (lines(GcalLampType.Arc) ++ lines(GcalLampType.Flat))
      .segmentN(4096)
      .flatMap { v => writer(v.force.toVector).transact(lxa) }

    for {
      _ <- IO(println(s"Importing $instFilePrefix ...")) // scalastyle:ignore
      _ <- unindexer.transact(lxa)
      _ <- prog.compile.drain
      _ <- indexer.transact(lxa)
    } yield ()
  }

  def runc: IO[Unit] =
    for {
      u <- UserDao.selectRootUser.transact(lxa)
      l <- Log.newLog[IO]("smartgcal importer", lxa)
      _ <- checkSmartDir
      _ <- IO(configureLogging)
      _ <- clean.transact(lxa)
      _ <- importAllInst
      _ <- l.shutdown(5 * 1000)
      _ <- IO(println("Done.")) // scalastyle:ignore
    } yield ()

  def main(args: Array[String]): Unit =
    runc.unsafeRunSync

}
