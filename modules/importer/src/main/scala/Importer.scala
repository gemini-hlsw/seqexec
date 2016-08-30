package gem

import gem.dao._
import gem.enum._
import gem.config._

import edu.gemini.pot.sp.{ISPProgram, ISPObservation}
import edu.gemini.spModel.io.SpImportService
import edu.gemini.spModel.core._
import edu.gemini.pot.spdb.{ IDBDatabaseService, DBLocalDatabase }
import edu.gemini.spModel.config.ConfigBridge
import edu.gemini.spModel.config.map.ConfigValMapInstances.IDENTITY_MAP;

import java.io._
import java.{ util => JU }
import java.util.logging.{ Logger, Level }

import scala.collection.JavaConverters._

import scalaz._, Scalaz._
import scalaz.effect._
import scalaz.std.effect.closeable._

import doobie.imports._

object Importer extends SafeApp {
  import Program.Id._
  // import ConfigSyntax._
  import ConfigReader3._

  val dir = new File("archive")

  val xa = DriverManagerTransactor[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  def steps(o: ISPObservation): List[Map[String, Object]] =
    ConfigBridge
      .extractSequence(o, new JU.HashMap, IDENTITY_MAP)
      .getAllSteps
      .toList
      .map { c =>
         c.itemEntries
          .iterator
          .map { e => (e.getKey.toString, e.getItemValue) }
          .toMap
      }

  def insert(p: ISPProgram): IO[Unit] = {
    val sid = p.getProgramID.toString
    val pid = Program.Id.parse(sid)
    val tit = p.getDataObject.getTitle
    val obs = p.getAllObservations.asScala.toList

    val ins: ConnectionIO[Unit] =
      ProgramDao.insert(Program(pid, tit, Nil)) *>
      obs.traverse_ { o =>

        val ss   = steps(o)
        val inst = ss.flatMap(_.cget(Legacy.Instrument.Instrument)).distinct match {
          case      Nil => None
          case o :: Nil => Some(o)
          case o :: os  => sys.error("More than one instrument in sequence: " + (o :: os))
        }

        val oid = o.getObservationID
        val newObs = Observation(
          Observation.Id(pid, o.getObservationNumber),
          o.getDataObject.getTitle,
          inst,
          Nil
        )

        val configs = ss.flatMap(unsafeFromConfig)

        ObservationDao.insert(newObs) *>
        configs.zipWithIndex.traverse { case (c, n) => StepDao.insert(newObs.id, n, c) }.void

      }

    IO.putStr(".") *>
    ins.transact(xa)
  }

  def readAndInsert(r: ProgramReader, f: File): IO[Unit] =
    r.read(f).flatMap {
      case Some(p) => insert(p).except(e => IO.putStrLn(">> " + p.getProgramID + ": " + e.getMessage))
      case None    => IO.ioUnit
    }

  def xmlFiles(dir: File, num: Int): IO[List[File]] =
    IO(dir.listFiles.toList.filter(_.getName.toLowerCase.endsWith(".xml"))).map(_.take(num))

  def readAndInsertAll(r: ProgramReader, dir: File, num: Int): IO[Unit] =
    xmlFiles(dir, num).flatMap(_.traverse_(readAndInsert(r, _)))

  val configLogging: IO[Unit] =
    IO(List(
      "edu.gemini.spModel.type.SpTypeUtil"
    ).map(Logger.getLogger).foreach(_.setLevel(Level.OFF)))

  val clean: ConnectionIO[Unit] =
    for {
      _ <- sql"truncate program cascade".update.run
      _ <- sql"delete from semester".update.run
    } yield ()

  val checkArchive: IO[Unit] =
    IO(dir.isDirectory).flatMap { b =>
      b.unlessM(IO(sys.error("""
        |
        |*** Root of project needs an archive/ dir with program xml files in it.
        |*** Try ln -s /path/to/some/stuff archive
        |""".stripMargin)))
    }

  override def runl(args: List[String]): IO[Unit] =
    for {
      n <- IO(args.headOption.map(_.toInt).getOrElse(Int.MaxValue))
      _ <- checkArchive
      _ <- configLogging
      _ <- clean.transact(xa)
      _ <- ProgramReader.using(readAndInsertAll(_, dir, n))
      _ <- IO.putStrLn("\nDone.")
    } yield ()


  def unsafeFromConfig(config: Map[String, Object]): Option[Step[InstrumentConfig]] = {

    val observeType = config.cget(Legacy.Observe.ObserveType)
    val instrument  = config.cget(Legacy.Instrument.Instrument)

    (observeType |@| instrument).tupled.collect {

      case ("BIAS",   i) =>
        BiasStep(unsafeInstConfig(i, config))

      case ("DARK",   i) =>
        DarkStep(unsafeInstConfig(i, config))

      case ("OBJECT" | "CAL", i) =>
        val p = config.cgetOrElse(Legacy.Telescope.P, OffsetP.Zero)
        val q = config.cgetOrElse(Legacy.Telescope.Q, OffsetQ.Zero)
        ScienceStep(unsafeInstConfig(i, config), TelescopeConfig(p,q))

      case ("ARC" | "FLAT", i) =>
        val l = config.uget(Legacy.Calibration.Lamp)
        val s = config.uget(Legacy.Calibration.Shutter)
        GcalStep(unsafeInstConfig(i, config), GcalConfig(l, s))

      case x =>
        sys.error("Unknown observeType: " + x + config.mkString("\n>  ", "\n>  ", ""))

    }

  }

  def unsafeInstConfig(i: Instrument, config: Map[String, Object]): InstrumentConfig =
    i match {
      case Instrument.Flamingos2 =>

        val fpu           = config.uget(Legacy.Instrument.F2.Fpu)
        val mosPreimaging = config.uget(Legacy.Instrument.MosPreImaging)
        val exposureTime  = config.uget(Legacy.Observe.ExposureTime)
        val filter        = config.uget(Legacy.Instrument.F2.Filter)
        val lyoutWheel    = config.uget(Legacy.Instrument.F2.LyotWheel)
        val disperser     = config.uget(Legacy.Instrument.F2.Disperser)
        F2Config(fpu, mosPreimaging, exposureTime, filter, lyoutWheel, disperser)

      case _ => GenericConfig(i)
    }

}
