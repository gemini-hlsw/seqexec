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
      case Shell => "\u001B[0;37m" // gray
    }
    EitherT.right {
      val pre = s"[${level.toString.take(4).toLowerCase}]"
      val messageColor = color // if (level == Shell) color else Console.BLUE
      for {
        i <- indent.read.map("  " * _)
        _ <- IO.putStrLn(f"$color$pre%-7s $messageColor$i$msg${Console.RESET}")
      } yield ()
    }
  }

  private def doShell(cmd: String \/ List[String], verbose: Boolean, indent: IORef[Int]): EitherT[IO, Int, Output] =
    for {
      o <- EitherT.right(IO.shell(cmd))
      _ <- verbose.whenM {
        doLog(Shell, s"$$ ${cmd.fold(identity, _.mkString(" "))}", indent) *>
        o.lines.traverseU(doLog(Shell, _, indent)) *>
        doLog(Shell, s"exit(${o.exitCode})", indent)
      }
    } yield o

  type Remote = String

  def interpreter(remote: Remote, verbose: Boolean, indent: IORef[Int]) = λ[CtlOp ~> EitherT[IO, Int, ?]] {
    case Shell(false, cmd) => doShell(cmd, verbose, indent)
    case Shell(true,  cmd) => doShell(cmd.bimap(s => s"ssh ${remote} $s", "ssh" :: remote :: _), verbose, indent)
    case Exit(exitCode)    => EitherT.left(exitCode.point[IO])
    case Gosub(level, msg, fa) =>
      for {
        _ <- doLog(level, msg, indent)
        _ <- EitherT.right(indent.mod(_ + 1))
        a <- fa.foldMap(this)
        _ <- EitherT.right(indent.mod(_ - 1))
      } yield a
  }

}
