import scalaz._, Scalaz._
import scalaz.effect._

import io._
import ctl._

object interp {

  private def doLog(level: Level, msg: String, indent: IORef[Int]): EitherT[IO, Int, Unit] = {
    val color = level match {
      case Error => Console.RED
      case Warn  => Console.YELLOW
      case Info  => Console.GREEN
    }
    EitherT.right {
      val pre = s"[${level.toString.toLowerCase}]"
      for {
        i <- indent.read.map { case 0 => "* " ; case n => " " * ((n + 1) * 2) }
        _ <- IO.putStrLn(f"$color$pre%-7s ${Console.RESET}$i$msg")
      } yield ()
    }
  }

  def interpreter(indent: IORef[Int]) = λ[CtlOp ~> EitherT[IO, Int, ?]] {
    case Log(level, msg) => doLog(level, msg, indent)
    case Shell(cmd)      => EitherT.right(IO.shell(cmd))
    case Exit(exitCode)  => EitherT.left(exitCode.point[IO])
    case Gosub(level, msg, fa) =>
      for {
        _ <- doLog(level, msg, indent)
        _ <- EitherT.right(indent.mod(_ + 1))
        a <- fa.foldMap(this)
        _ <- EitherT.right(indent.mod(_ - 1))
      } yield a
  }

}
