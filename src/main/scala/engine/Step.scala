package engine

import scala.concurrent.ExecutionContext

import cats.implicits._
import cats.effect.Effect

import fs2.async

sealed trait Step
object Step {

  // Whatever is shared among all Steps. Notice it doesn't // extend Step
  // because it's meant to be used only internally
  case class Core(
    id: String,
    config: Core.Configuration
  ) {

    // Shared Resources, static
    val resources: Set[Resource] = ???

  }

  object Core {
    case class Configuration()
  }

  sealed trait F2 extends Step
  object F2 {

    sealed trait Pending extends F2 {
      def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Either[Failed, Done]]
    }

    object Pending {

      case class Standard(core: Core, breakpoint: Boolean) extends Pending {

        override def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Either[Failed, Done]] = ???
        //   // Alternative:
        //   // async.parallelSequence(List(Execution.configureTCS, Execution.configureInst("F2"))).flatMap(...)
        //   (for {
        //      t1 <- async.start(
        //        Execution.configureTCS
        //          //.flatMap(// x => sig.modify(Sequence.State.current.set(Some(Right(Ongoing())))) *> F.pure(x))
        //      )
        //      t2 <- async.start(Execution.configureInst("F2"))
        //      r1 <- t1
        //      r2 <- t2
        //    } yield (r1, r2)).flatMap {
        //     case (Right(_), Right(_)) =>
        //         Execution.observe.map {
        //           case Right(_) => ??? // Right(Done())
        //           case Left(_) => ??? // Left(Failed())
        //         }
        //     case _ => ??? // F.pure(Left(Failed()))
        //   }
        // }
      }

      case class Flat(core: Core) extends Pending {
        override def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Either[Failed, Done]] = ???
      }

      case class Dark(core: Core) extends Pending {
        override def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Either[Failed, Done]] = ???
      }

    }

    case class Done(core: Core, fileId: String) extends F2
    case class Ongoing(core: Core, progress: Int) extends F2
    case class Failed(core: Core, message: String) extends F2

    sealed trait Current extends F2 { self =>
      def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Unit] = self match {
        case Current.Pending(pending) =>
          pending.execute(m) match {
            case _ => ???
          }
        case Current.Ongoing(ongoing) =>
          F.pure(Unit) // Already executing Step don't do anything
        case Current.Failed(failed) => ??? // Leave it as failed and stop
      }
      // current match {
      //   // No current Step
      //   case None => pending match {
      //     // No pending steps, done
      //     case Nil => m.setStatus(Status.Finished).void
      //     // More pending steps
      //     case (next :: remainder) => next.execute(m).flatMap {
      //       case Left(e) => m.setState(
      //         Sequence.State(
      //           F2(done, remainder, Some(Left(e))),
      //           Status.Failed
      //         )
      //       )
      //       case Right(d) => m.setSequence(
      //         F2((d :: done), remainder, None)
      //       ).flatMap(_.sequence.execute(m))
      //     }
      //   }
      //   case Some(_) => F.pure(Unit) // Event: Tried execute Sequence with an already ongoing step.
      // }
    }

    object Current {
      case class Pending(step: F2.Pending) extends Current
      case class Ongoing(current: F2.Ongoing) extends Current
      case class Failed(current: F2.Failed) extends Current
    }

  }

  sealed trait GMOS extends Step
  case object GMOS

}
