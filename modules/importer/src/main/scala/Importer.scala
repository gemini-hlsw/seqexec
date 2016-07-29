package gem

import gem.dao._

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

  val dir = new File("/Users/rnorris/Scala/ocs-arch/20140922-0730")

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
          case o :: Nil => Some(o.asInstanceOf[String])
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

    IO.putStrLn(sid + " - " + tit) *> ins.transact(xa)
  }



  def readAndInsert(r: ProgramReader, f: File): IO[Unit] =
    r.read(f).flatMap(insert).except(e => IO.putStrLn(">> " + e.getMessage))

  def xmlFiles(dir: File): IO[List[File]] =
    IO(dir.listFiles.toList.filter(_.getName.toLowerCase.endsWith(".xml"))).map(_.take(10))

  def readAndInsertAll(r: ProgramReader, dir: File): IO[Unit] =
    xmlFiles(dir).flatMap(_.traverse_(readAndInsert(r, _)))

  val configLogging: IO[Unit] =
    IO(List(
      "edu.gemini.spModel.type.SpTypeUtil"
    ).map(Logger.getLogger).foreach(_.setLevel(Level.OFF)))

  val clean: ConnectionIO[Unit] =
    for {
      _ <- sql"truncate program cascade".update.run
      _ <- sql"delete from semester".update.run
    } yield ()

  override def runc: IO[Unit] =
    configLogging      *>
    clean.transact(xa) *>
    ProgramReader.using(readAndInsertAll(_, dir))


  private implicit class ConfigOps(m: Map[String, Object]) {
    def nn[A](key: String): A = op(key).getOrElse(sys.error("config key not found: " + key))
    def op[A](key: String): Option[A] = m.get(key).map(_.asInstanceOf[A])
    def oe[A: Enumerated](key: String): Option[A] = op[String](key).map(Enumerated[A].unsafeFromTag)
  }

  def unsafeFromConfig(config: Map[String, Object]): Option[seq.Step[_ <: seq.Instrument]] = {
    val observeType  = config.op[String    ]("observe:observeType"  )
    val instrument   = config.oe[Instrument]("instrument:instrument")

    (observeType |@| instrument).tupled.collect { 
      case ("BIAS",   i) => seq.BiasStep(new seq.Instrument { val tag = i.tag.toString })
      case ("DARK",   i) => seq.DarkStep(new seq.Instrument { val tag = i.tag.toString })
      case ("OBJECT", i) => 

        val p = config.op[String]("telescope:p").map(s => OffsetP(Angle.fromArcsecs(s.toDouble))).getOrElse(OffsetP.Zero)
        val q = config.op[String]("telescope:q").map(s => OffsetQ(Angle.fromArcsecs(s.toDouble))).getOrElse(OffsetQ.Zero)

        // config.filterKeys(_.startsWith("telescope:")).foreach(println)
        seq.ScienceStep(new seq.Instrument { val tag = i.tag.toString }, seq.Telescope(p,q))
    }

  }
    // new Step {
    //   val stepCount    = config.nn[Int       ]("metadata:stepcount"   )
    //   val isComplete   = config.nn[Boolean   ]("metadata:complete"    )
    //   val observeClass = config.oe[ObsClass  ]("observe:class"        )
    //   val dataLabel    = config.nn[String    ]("observe:dataLabel"    )
    //   val target       = config.op[String    ]("observe:object"       )
    //   val observeType  = config.op[String    ]("observe:observeType"  )
    //   val band         = config.op[Int       ]("observe:sciBand"      )
    //   val instrument   = config.oe[Instrument]("instrument:instrument")
    //   val id = StepId(oid, stepCount)
    // }

}

