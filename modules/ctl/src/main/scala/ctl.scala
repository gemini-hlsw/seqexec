import scalaz._, Scalaz._
import io._

object ctl {

  sealed trait Level
  case object Info  extends Level
  case object Warn  extends Level
  case object Error extends Level


  sealed trait CtlOp[A]
  case class Shell(cmd: String \/ List[String]) extends CtlOp[Output]
  case class Log(level: Level, msg: String) extends CtlOp[Unit]
  case class Exit[A](exitCode: Int) extends CtlOp[A]
  case class Gosub[A](level: Level, msg: String, fa: CtlIO[A]) extends CtlOp[A]

  type CtlIO[A] = Free[CtlOp, A]

  def shell(c: String):               CtlIO[Output] = Free.liftF(Shell(c.left))
  def shell(c: String, cs: String*):  CtlIO[Output] = Free.liftF(Shell((c :: cs.toList).right))
  def log(level: Level, msg: String): CtlIO[Unit]   = Free.liftF(Log(level, msg))
  def exit[A](exitCode: Int):         CtlIO[A]      = Free.liftF(Exit[A](exitCode))

  def gosub[A](level: Level, msg: String, fa: CtlIO[A]): CtlIO[A]   = Free.liftF(Gosub(level, msg, fa))

  def require[A](shell: CtlIO[Output])(f: PartialFunction[Output, A]): CtlIO[A] =
    shell.flatMap { o =>
      f.lift(o) match {
        case None    =>
          o.lines.traverse(log(Error, _))      *>
          log(Error, s"exited (${o.exitCode})") *>
          exit[A](o.exitCode)
        case Some(a) => a.point[CtlIO]
      }
    }

  implicit class CtlIOOps[A](fa: CtlIO[A]) {
    def require[B](f: PartialFunction[Output, B])(implicit ev: A =:= Output): CtlIO[B] =
      ctl.require(fa.map(ev))(f)
  }


}
