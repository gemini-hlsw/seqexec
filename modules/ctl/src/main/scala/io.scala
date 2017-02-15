import scalaz._
import scalaz.effect._

object io {

  case class Output(exitCode: Int, lines: List[String])

  implicit class MoreIOConstructors(module: IO.type) {
    def shell(cmd: String \/ List[String]): IO[Output] =
      IO {
        import scala.sys.process._
        import collection.mutable.ListBuffer
        val b = ListBuffer[String]()
        val x = cmd.fold(_.cat, _.cat).run(ProcessLogger(b append _)).exitValue
        Output(x, b.toList)
      }
  }

}
