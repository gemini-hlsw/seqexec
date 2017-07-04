import scala.concurrent.ExecutionContext

import cats.Apply
import cats.implicits._
import cats.effect.Effect
// import cats.effect.implicits._

import fs2.Stream
import fs2.async
import fs2.async.mutable.{Signal, Queue}

package object engine {

  def run[F[_]](in: Queue[F, Event])(st0: Sequence.State)(implicit F: Effect[F], ec: ExecutionContext): Stream[F, Sequence.State] =
    Stream.force(
      async.signalOf[F, Sequence.State](st0).flatMap(sig =>
        async.fork(forever(reader(sig)(in))) *> sig.discrete.pure[F]
      )
    )

  def reader[F[_]](sig: Signal[F, Sequence.State])(in: Queue[F, Event])(implicit F: Effect[F], ec: ExecutionContext): F[Unit] =
    in.dequeue1.flatMap {
      case Event.Start => sig.get.flatMap(st =>
        st.status match {
          case Status.Waiting =>
            sig.set(Sequence.State.status.set(Status.Running)(st)) *>
              async.fork(st.sequence.execute(sig))
          case _ => F.pure(Unit) // Event: Status is not waiting, dont't execute
        }
      )

    case Event.Stop  => sig.get.flatMap { st =>
      st.status match {
        case Status.Running => sig.set(Sequence.State.status.set(Status.Waiting)(st))
        // Leave status as it is
        case _ => F.pure(Unit) // Event: Status not Running, no need to stop
      }
    }
  }

  // This won't be accepted upstream because it would have to be Lazy (ApplyLazy
  // typeclass would be needed too)
  private def forever[F[_], A, B](fa: F[A])(implicit F: Apply[F]): F[B] = fa *> forever(fa)
}
