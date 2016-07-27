package gem

import edu.gemini.pot.sp.ISPProgram
import edu.gemini.spModel.io.impl.PioSpXmlParser
import edu.gemini.spModel.io.SpImportService
import edu.gemini.spModel.core.ProgramId
import edu.gemini.spModel.core.ProgramId._
import edu.gemini.pot.spdb.{ IDBDatabaseService, DBLocalDatabase }

import java.io._
import java.util.logging.{ Logger, Level }

import scalaz.Scalaz._
import scalaz.effect._
import scalaz.std.effect.closeable._

case class ProgramReader(parser: PioSpXmlParser) {
  def read(f: File): IO[ISPProgram] =
    IO(new FileInputStream(f)).using { fis =>
      IO(new InputStreamReader(fis, "UTF-8")).using { r =>
        IO {
          parser.parseDocument(r) match {
            case p: ISPProgram => p 
            case _             => sys.error("Not a science program: " + f)
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