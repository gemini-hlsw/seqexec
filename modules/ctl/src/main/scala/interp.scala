import scalaz._, Scalaz._
import scalaz.effect._

import io._
import ctl._
import opts.Config

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

  private def doShell(cmd: String \/ List[String], verbose: Boolean, indent: IORef[Int]): EitherT[IO, Int, Output] = {

    def handler(s: String): IO[Unit] =
      if (verbose) doLog(Shell, s, indent).run.map(_.toOption.get) // shh
      else IO.putStr(".")

    for {
      _ <- verbose.whenM(doLog(Shell, s"$$ ${cmd.fold(identity, _.mkString(" "))}", indent))
      o <- EitherT.right(IO.shell(cmd, handler))
      _ <- verbose.whenM(doLog(Shell, s"exit(${o.exitCode})", indent))
      - <- verbose.unlessM(EitherT.right[IO, Int, Unit](IO.putStr("\u001B[0E")))
    } yield o

  }

  def interpreter(c: Config, indent: IORef[Int]) = λ[CtlOp ~> EitherT[IO, Int, ?]] {
    case Shell(false, cmd) => doShell(cmd, c.verbose, indent)
    case Shell(true,  cmd) => doShell(cmd.bimap(s => s"ssh ${c.userAndHost.userAndHost} $s", "ssh" :: c.userAndHost.userAndHost :: _), c.verbose, indent)
    case Exit(exitCode)    => EitherT.left(exitCode.point[IO])
    case GetConfig         => c.point[EitherT[IO, Int, ?]]
    case Gosub(level, msg, fa) =>
      for {
        _ <- doLog(level, msg, indent)
        _ <- EitherT.right(indent.mod(_ + 1))
        a <- fa.foldMap(this)
        _ <- EitherT.right(indent.mod(_ - 1))
      } yield a
  }

}
