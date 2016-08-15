package gem

import gem.dao._
import gem.enum._

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
  import ConfigSyntax._

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
      ProgramDao.insert(Program(pid, tit)) *>
      obs.traverse_ { o => 

        val ss   = steps(o)
        val inst = ss.flatMap(_.get("instrument:instrument")).distinct match {
          case      Nil => None
          case o :: Nil => Some(o.asInstanceOf[String]).map(s => Instrument.all.find(_.tccValue == s).getOrElse(sys.error("Invalid inst tccValue: " + s)))
          case o :: os  => sys.error("More than one instrument in sequence: " + (o :: os))
        } 

        val oid = o.getObservationID
        val newObs = Observation(
          Observation.Id(pid, o.getObservationNumber),
          o.getDataObject.getTitle,
          inst
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

  // 

  def unsafeFromConfig(config: Map[String, Object]): Option[seq.Step[_ <: seq.Instrument]] = {

    val observeType  = config.read[Option[String    ]]("observe:observeType"  )
    val instrument   = config.read[Option[Instrument]]("instrument:instrument")

    (observeType |@| instrument).tupled.collect {
      
      case ("BIAS",   i) =>
        seq.BiasStep(new seq.Instrument { val tag = i.tag.toString })

      case ("DARK",   i) => 
        seq.DarkStep(new seq.Instrument { val tag = i.tag.toString })

      case ("OBJECT" | "CAL", i) =>
        val p = config.read[Option[OffsetP]]("telescope:p").getOrElse(OffsetP.Zero)
        val q = config.read[Option[OffsetQ]]("telescope:q").getOrElse(OffsetQ.Zero)
        seq.ScienceStep(new seq.Instrument { val tag = i.tag.toString }, seq.Telescope(p,q))
        
      case ("ARC" | "FLAT", i) =>
        val l = config.read[GCalLamp]("calibration:lamp")
        val s = config.read[GCalShutter]("calibration:shutter")
        seq.GcalStep(new seq.Instrument { val tag = i.tag.toString }, seq.GcalUnit(l, s))

      case x => 
        sys.error("Unknown observeType: " + x + config.mkString("\n>  ", "\n>  ", ""))

    }

  }


}

