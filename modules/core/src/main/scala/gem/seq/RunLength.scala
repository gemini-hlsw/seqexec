// package gem.seq

// import scalaz._, Scalaz._

// final class RunLength[+A] private (val data: List[(Int, A)]) extends Serializable {

//   def uncons[B](f: => B)(g: (A, RunLength[A]) => B): B =
//     data match {
//       case Nil          => f
//       case (1, a) :: ps => g(a, new RunLength(ps))
//       case (n, a) :: ps => g(a, new RunLength((n - 1, a) :: ps))
//     }

//   def ::[B >: A](b: B)(implicit ev: Equal[B]): RunLength[B] =
//     data match {
//       case (n, b1) :: ps if ev.equal(b, b1) => new RunLength((n + 1, b) :: ps)
//       case ps                               => new RunLength((    1, b) :: ps)
//     }

//   def toList: List[A] =
//     data.flatMap { case (n, a) =>
//       List.fill(n)(a)
//     }

//   // note: not a functor
//   def map[B: Equal](f: A => B) =
//     RunLength.fromFoldable(toList.map(f))

//   // note: not a monad
//   def flatMap[B: Equal](f: A => RunLength[B]) =
//     RunLength.fromFoldable(toList.flatMap(a => f(a).toList))

//   def filter(f: A => Boolean) =
//     new RunLength(data.filter { case (_, a) => f(a) })

//   override def toString: String =
//     data.mkString("RunLength(", ", ", ")")

// }

// object RunLength {

//   def empty[A]: RunLength[A] =
//     new RunLength(List.empty[(Int, A)])

//   def fromFoldable[F[_]: Foldable, A: Equal](as: F[A]): RunLength[A] =
//     as.foldRight(RunLength.empty[A])(_ :: _)

//   def fromList[A: Equal](as: List[A]): RunLength[A] =
//     fromFoldable(as)

//   def apply[A: Equal](as: A*): RunLength[A] =
//     as.foldRight(RunLength.empty[A])(_ :: _)

//   implicit val FoldableRunLength: Foldable[RunLength] =
//     new Foldable.FromFoldr[RunLength] {
//       def foldRight[A, B](fa: RunLength[A], z: => B)(f: (A, => B) => B): B =
//         fa.toList.foldRight(z)(f(_, _))
//     }

// }
