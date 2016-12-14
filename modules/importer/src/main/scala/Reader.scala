package gem

import edu.gemini.pot.sp.ISPProgram
import edu.gemini.spModel.io.impl.PioSpXmlParser
import edu.gemini.pot.spdb.{ IDBDatabaseService, DBLocalDatabase }

import java.io._

import scalaz.Scalaz._
import scalaz.effect._
import scalaz.std.effect.closeable._

case class ProgramReader(parser: PioSpXmlParser) {
  def read(f: File): IO[Option[ISPProgram]] =
    IO(new FileInputStream(f)).using { fis =>
      IO(new InputStreamReader(fis, "UTF-8")).using { r =>
        IO {
          parser.parseDocument(r) match {
            case p: ISPProgram => Some(p)
            case _             => None // not a science program
          }
        }
      }
    }
}

object ProgramReader {

  implicit val IDBDatabaseServiceResource: Resource[IDBDatabaseService] =
    new Resource[IDBDatabaseService] {
      def close(db: IDBDatabaseService) = IO(db.getDBAdmin.shutdown())
    }

  def forDatabase(db: IDBDatabaseService): ProgramReader =
    ProgramReader(new PioSpXmlParser(db.getFactory))

  def using[A](f: ProgramReader => IO[A]): IO[A] =
    IO(DBLocalDatabase.createTransient).using(f.contramap(forDatabase))

}