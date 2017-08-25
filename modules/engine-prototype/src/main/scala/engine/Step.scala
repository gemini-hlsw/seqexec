package engine

import scala.concurrent.ExecutionContext

import cats.implicits._
import cats.effect.Effect

sealed trait Step
object Step {

  // Whatever is shared among all Steps. Notice it doesn't // extend Step
  // because it's meant to be used only internally
  case class Core(
    id: String,
    config: Core.Configuration,
    breakpoint: Boolean
  )

  object Core {
    case class Configuration()
  }

  sealed trait F2 extends Step
  object F2 {

    case class Done(core: Core, fileId: String) extends F2

    sealed trait Pending extends F2
    object Pending {
      case class Standard(core: Core) extends Pending
      case class Flat(core: Core) extends Pending
      case class Dark(core: Core) extends Pending
    }

    sealed trait Ongoing extends F2
    object Ongoing {
      final case class Standard(core: Core, tcsConfigured: Boolean, instConfigured: Boolean) extends Ongoing
    }

    sealed trait Failed extends F2
    object Failed {
      final case class Standard(core: Core, tcsFailed: Boolean, instFailed: Boolean, observationFailed: Boolean) extends F2
      // TODO: final case class Standard(core: Core, tcsFailure: Option[Failure], instFailure: Option[Failure]) extends F2
    }

    sealed trait Current extends F2 {

      // Accessor to the core Step
      val core: Core = ???

    }

    object Current {
      // Only a Pending Step in Current can be executed
      sealed trait Pending extends Current {

        def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Unit]

      }

      object Pending {

        case class Standard(step: F2.Pending.Standard) extends Pending {

          def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Unit] = {

            def configureTCS: F[Boolean] =
              Execution.configureTCS.flatMap {
                case Right(_) =>
                  m.modify(
                    Sequence.State.current.modify {
                      case Pending.Standard(s) => Ongoing.Standard(
                        F2.Ongoing.Standard(s.core, tcsConfigured = true, instConfigured = false)
                      )
                      case Ongoing.Standard(s) => Ongoing.Standard(
                        F2.Ongoing.Standard(s.core, tcsConfigured = true, instConfigured = true)
                      )
                      case Failed.Standard(s) => Failed.Standard(
                        F2.Failed.Standard(
                          s.core,
                          tcsFailed = false,
                          instFailed = true,
                          observationFailed = true
                        )
                      ) // Instrument already failed
                    }
                  ) *> true.pure[F]

                case Left(_) =>
                  m.modify(
                    Sequence.State.current.modify {
                      case Failed.Standard(s) => Failed.Standard(
                        F2.Failed.Standard(
                          s.core,
                          tcsFailed = true,
                          instFailed = true,
                          observationFailed = false
                        )
                      )
                      // Instrument didn't fail yet
                      case s => Failed.Standard(
                        F2.Failed.Standard(
                          s.core,
                          tcsFailed = true,
                          instFailed = false,
                          observationFailed = false
                        )
                      )
                    }
                  ) *> false.pure[F]

              }

            def configureInst: F[Boolean] =
              Execution.configureInst("F2").flatMap {
                case Right(_) =>
                  m.modify(
                    Sequence.State.current.modify {
                      case Pending.Standard(s) => Ongoing.Standard(
                        F2.Ongoing.Standard(s.core, tcsConfigured = false, instConfigured = true)
                      )
                      case Ongoing.Standard(s) => Ongoing.Standard(
                        F2.Ongoing.Standard(s.core, tcsConfigured = true, instConfigured = true)
                      )
                      case Failed.Standard(s) => Failed.Standard(
                        F2.Failed.Standard(
                          s.core,
                          tcsFailed = true,
                          instFailed = false,
                          observationFailed = false
                        )
                      ) // Instrument already failed
                    }
                  ) *> true.pure[F]

                case Left(_) =>
                  m.modify(
                    Sequence.State.current.modify {
                      case Failed.Standard(s) => Failed.Standard(
                        F2.Failed.Standard(
                          s.core,
                          tcsFailed = true,
                          instFailed = true,
                          observationFailed = false
                        )
                      )
                      // TCS didn't fail yet
                      case s => Failed.Standard(
                        F2.Failed.Standard(
                          s.core,
                          tcsFailed = false,
                          instFailed = true,
                          observationFailed = false
                        )
                      )
                    }
                  ) *> false.pure[F]
              }

              // XXX: Check running status
              both(configureTCS, configureInst).flatMap {
              case (true, true) =>
                  // XXX: Check running status
                  Execution.observe.flatMap {
                    case Some(fileId) => execute(m) // XXX: Shift current focus
                    case None =>
                      m.modify(
                        Sequence.State.current.modify(s =>
                          Failed.Standard(
                            F2.Failed.Standard(
                              s.core,
                              tcsFailed = false,
                              instFailed = false,
                              observationFailed = true
                            )
                          )
                        )
                      ).void
                  }

              case (_, _) => F.pure(Unit)
            }
          }
        }
      }

      sealed trait Ongoing extends Current
      object Ongoing {
        case class Standard(step: F2.Ongoing.Standard) extends Ongoing
      }

      sealed trait Failed extends Current
      object Failed {
        case class Standard(step: F2.Failed.Standard) extends Failed
      }
    }

  }

  sealed trait GMOS extends Step
  case object GMOS

  // This won't be accepted in cats-effect
  // https://gist.github.com/djspiewak/a775b73804c581f4028fea2e98482b3c but with
  // fs2 it should be possible, `parallelSequence` is already there.
  private def both[F[_], A, B](fa: F[A], fb: F[B])(implicit F: Effect[F]): F[(A, B)] = ???

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
// }


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
