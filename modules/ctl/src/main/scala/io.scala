import scalaz._
import scalaz.effect._

object io {

  case class Output(exitCode: Int, lines: List[String])

  implicit class MoreIOConstructors(module: IO.type) {

    def shell(cmd: String \/ List[String], f: String => IO[Unit] = (_ => IO.ioUnit)): IO[Output] =
      IO {
        import scala.sys.process._
        import collection.mutable.ListBuffer
        val b = ListBuffer[String]()
        val x = cmd.fold(_.cat, _.cat).run(ProcessLogger { s =>
          f(s).unsafePerformIO // shh
          b.append(s) // side-effect
        }).exitValue
        Output(x, b.toList)
      }

  }

}
