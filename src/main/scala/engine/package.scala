import scala.concurrent.ExecutionContext

import cats.Apply
import cats.implicits._
import cats.effect.Effect

import fs2.Stream
import fs2.async
import fs2.async.mutable.Queue

package object engine {

  def run[F[_]](in: Queue[F, Event])(st0: Sequence.State)(implicit F: Effect[F], ec: ExecutionContext): Stream[F, Sequence.State] =
    Sequence.State.Mutable.stream[F](st0: Sequence.State)(m => forever(reader(in)(m)))

  def reader[F[_]](in: Queue[F, Event])(m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Unit] =
    in.dequeue1.flatMap {
      case Event.Start => m.withState(st0 =>
        st0.status match {
          case Status.Waiting => {
            val st1 = Sequence.State.status.set(Status.Running)(st0)
            async.fork(st1.sequence.execute(m)) *> F.pure(st1)
          }
          case _ => F.pure(st0) // Event: Status not Waiting, dont't execute
        }
      ).void

    case Event.Stop  => m.modify(st =>
      st.status match {
        case Status.Running => Sequence.State.status.set(Status.Waiting)(st)
        // Leave status as it is
        case _ => st // Event: Status not Running, no need to stop
      }
    ).void
  }

  // This won't be accepted upstream because it would have to be Lazy (ApplyLazy
  // typeclass would be needed too)
  private def forever[F[_], A, B](fa: F[A])(implicit F: Apply[F]): F[B] = fa *> forever(fa)
}
