package gem

import gem.config.{GcalConfig, F2SmartGcalKey, SmartGcalKey}
import gem.dao.{SmartGcalDao, UserDao}
import gem.enum.{GcalBaselineType, GcalLampType}

import java.io.File
import java.time.Duration

import doobie.imports._

import scala.io.Source

import scalaz._, Scalaz._
import scalaz.effect._

object SmartGcalImporter extends SafeApp {

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

  import ConfigReader.Legacy._

  def parseF2(input: List[String]): (SmartGcalKey, List[String]) = {
    import edu.gemini.spModel.gemini.flamingos2.{Flamingos2 => OldF2}
    import Instrument.F2._

    val disperserS :: filterS :: fpuS :: gcal = input

    val d = Disperser.read(OldF2.Disperser.byName(disperserS).getValue)
    val f = Filter.read(OldF2.Filter.byName(filterS).getValue)
    val u = Fpu.read(OldF2.FPUnit.byName(fpuS).getValue)

    (F2SmartGcalKey(d, f, u), gcal)
  }

  // -------------------------------------------------------------------------
  // Implementation Details
  // -------------------------------------------------------------------------

  val dir = new File("smartgcal")
  val xa  = Importer.xa
  val lxa = Importer.lxa

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
    smartTables.map(_.tableName).traverseU(t => Update0(s"truncate $t cascade", None).run).void

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
    import edu.gemini.spModel.gemini.calunit.{CalUnitParams => OldGcal}
    import Calibration._

    val _ :: filterS :: diffuserS :: lampS :: shutterS :: expS :: coaddsS :: baselineS :: Nil = input

    val l = Lamp.read(new java.util.HashSet(OldGcal.Lamp.read(lampS)))
    val f = Filter.read(OldGcal.Filter.getFilter(filterS))
    val d = Diffuser.read(OldGcal.Diffuser.getDiffuser(diffuserS))
    val s = Shutter.read(OldGcal.Shutter.getShutter(shutterS))
    val e = Duration.ofMillis(expS.toLong * 1000)
    val c = coaddsS.toInt

    val b = GcalBaselineType.unsafeFromTag(baselineS)

    (b, GcalConfig(l, f, d, s, e, c))
  }

  def parseFile(input: List[String], l: GcalLampType, parser: KeyParser): (GcalLampType, GcalBaselineType, SmartGcalKey, GcalConfig) = {
    val (k, r) = parser(input)
    val (b, g) = parseGcal(r)
    (l, b, k, g)
  }

  def loadCsv(fileNamePrefix: String, lampType: GcalLampType, parser: KeyParser): IO[Unit] =
    for {
      lines <- readSmartGcalFile(new File(dir, s"${fileNamePrefix}_${lampType.tag.toUpperCase}.csv"))
      rows   = lines.map(parseFile(_, lampType, parser))
      _     <- rows.traverseU { case (l, b, k, g) => SmartGcalDao.insert(l, b, k, g) }.transact(xa)
    } yield ()

  def loadInstrument(fileNamePrefix: String, parser: KeyParser): IO[Unit] =
    GcalLampType.all.traverseU(t => loadCsv(fileNamePrefix, t, parser)).void

  override def runl(args: List[String]): IO[Unit] =
    for {
      u <- UserDao.selectRoot.transact(xa)
      l <- Log.newLog[IO]("smartgcal importer", lxa)
      _ <- checkSmartDir
      _ <- Importer.configLogging
      _ <- clean.transact(xa)
      _ <- smartTables.traverseU(d => loadInstrument(d.filePrefix, d.parser))
      _ <- l.shutdown(5 * 1000)
      _ <- IO.putStrLn("Done.")
    } yield ()
}
