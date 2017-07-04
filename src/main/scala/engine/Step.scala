package engine

import scala.concurrent.ExecutionContext

import cats.implicits._
import cats.effect.Effect

import fs2.async
import fs2.async.mutable.Signal

sealed trait Step
object Step {

  sealed trait F2 extends Step
  object F2 {

    sealed trait Pending {
      def execute[F[_]](sig: Signal[F, Sequence.State])(implicit F: Effect[F], ec: ExecutionContext): F[Either[Failed, Done]]
    }
    object Pending {
      case class Flat() extends Pending {
        override def execute[F[_]](sig: Signal[F, Sequence.State])(implicit F: Effect[F], ec: ExecutionContext): F[Either[Failed, Done]] = {
          // TODO: Alternative:
          // async.parallelSequence(List(Execution.configureTCS, Execution.configureInst("F2"))).flatMap(...)
          (for {
             t1 <- async.start(
               Execution.configureTCS.flatMap(x =>
                 sig.modify(Sequence.State.current.set(Some(Right(Ongoing())))) *> F.pure(x)
               )
             )
             t2 <- async.start(Execution.configureInst("F2"))
             r1 <- t1
             r2 <- t2
           } yield (r1, r2)).flatMap {
            case (Right(_), Right(_)) =>
                Execution.observe.map {
                  case Right(_) => Right(Done())
                  case Left(_) => Left(Failed())
                }
            case _ => F.pure(Left(Failed()))
          }
        }
      }

      case class Dark() extends Pending {
        override def execute[F[_]](sig: Signal[F, Sequence.State])(implicit F: Effect[F], ec: ExecutionContext): F[Either[Failed, Done]] = ???
      }

    }

    case class Done() extends F2
    case class Ongoing() extends F2
    case class Failed() extends F2
  }

  sealed trait GMOS extends Step
  case object GMOS
}
