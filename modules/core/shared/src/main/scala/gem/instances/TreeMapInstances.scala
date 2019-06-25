// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.instances

import cats.{Always, Applicative, Eval, FlatMap, Foldable, Monoid, Show, Traverse}
import cats.kernel._
import cats.kernel.instances.StaticMethods
import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

// This redefines all the cats SortedMap instances for TreeMap. It's a copy/paste from the cats
// source. We may want to go back and do everything in terms of SortedMap … not sure we need
// TreeMap specifically. TBD.
trait TreeMapInstances extends TreeMapInstances2 {

  implicit def catsStdHashForTreeMap[K: Hash: Order, V: Hash]: Hash[TreeMap[K, V]] =
    new TreeMapHash[K, V]

  implicit def catsStdCommutativeMonoidForTreeMap[K: Order, V: CommutativeSemigroup]: CommutativeMonoid[TreeMap[K, V]] =
    new TreeMapCommutativeMonoid[K, V]

  implicit def catsStdShowForTreeMap[A: Order, B](implicit showA: Show[A], showB: Show[B]): Show[TreeMap[A, B]] =
    new Show[TreeMap[A, B]] {
      def show(m: TreeMap[A, B]): String =
        m.iterator
          .map { case (a, b) => showA.show(a) + " -> " + showB.show(b) }
          .mkString("TreeMap(", ", ", ")")
    }

  implicit def catsStdInstancesForTreeMap[K: Order]: Traverse[TreeMap[K, ?]] with FlatMap[TreeMap[K, ?]] =
    new Traverse[TreeMap[K, ?]] with FlatMap[TreeMap[K, ?]] {

      implicit val orderingK: Ordering[K] = Order[K].toOrdering

      def traverse[G[_], A, B](fa: TreeMap[K, A])(f: A => G[B])(implicit G: Applicative[G]): G[TreeMap[K, B]] = {
        val gba: Eval[G[TreeMap[K, B]]] = Always(G.pure(TreeMap.empty(Order[K].toOrdering)))
        Foldable.iterateRight(fa, gba){ (kv, lbuf) =>
          G.map2Eval(f(kv._2), lbuf)({ (b, buf) => buf + (kv._1 -> b)})
        }.value
      }

      def flatMap[A, B](fa: TreeMap[K, A])(f: A => TreeMap[K, B]): TreeMap[K, B] =
        fa.flatMap { case (k, a) => f(a).get(k).map((k, _)) }

      override def map[A, B](fa: TreeMap[K, A])(f: A => B): TreeMap[K, B] =
        fa.map { case (k, a) => (k, f(a)) }


      override def map2Eval[A, B, Z](fa: TreeMap[K, A], fb: Eval[TreeMap[K, B]])(f: (A, B) => Z): Eval[TreeMap[K, Z]] =
        if (fa.isEmpty) Eval.now(TreeMap.empty(Order[K].toOrdering)) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      override def ap2[A, B, Z](f: TreeMap[K, (A, B) => Z])(fa: TreeMap[K, A], fb: TreeMap[K, B]): TreeMap[K, Z] =
        f.flatMap { case (k, f) =>
          for { a <- fa.get(k); b <- fb.get(k) } yield (k, f(a, b))
        }

      def foldLeft[A, B](fa: TreeMap[K, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b) { case (x, (_, a)) => f(x, a)}

      def foldRight[A, B](fa: TreeMap[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.values, lb)(f)

      def tailRecM[A, B](a: A)(f: A => TreeMap[K, Either[A, B]]): TreeMap[K, B] = {
        val bldr = TreeMap.newBuilder[K, B](Order[K].toOrdering)

        @tailrec def descend(k: K, either: Either[A, B]): Unit =
          either match {
            case Left(a) =>
              f(a).get(k) match {
                case Some(x) => descend(k, x)
                case None => ()
              }
            case Right(b) =>
              bldr += ((k, b))
              ()
          }

        f(a).foreach { case (k, a) => descend(k, a) }
        bldr.result
      }

      override def size[A](fa: TreeMap[K, A]): Long = fa.size.toLong

      override def get[A](fa: TreeMap[K, A])(idx: Long): Option[A] =
        if (idx < 0L || Int.MaxValue < idx) None
        else {
          val n = idx.toInt
          if (n >= fa.size) None
          else Some(fa.valuesIterator.drop(n).next)
        }

      override def isEmpty[A](fa: TreeMap[K, A]): Boolean = fa.isEmpty

      override def fold[A](fa: TreeMap[K, A])(implicit A: Monoid[A]): A =
        A.combineAll(fa.values)

      override def toList[A](fa: TreeMap[K, A]): List[A] = fa.values.toList

      override def collectFirst[A, B](fa: TreeMap[K, A])(pf: PartialFunction[A, B]): Option[B] = fa.collectFirst(new PartialFunction[(K, A), B] {
        override def isDefinedAt(x: (K, A)) = pf.isDefinedAt(x._2)
        override def apply(v1: (K, A)) = pf(v1._2)
      })

      override def collectFirstSome[A, B](fa: TreeMap[K, A])(f: A => Option[B]): Option[B] = collectFirst(fa)(Function.unlift(f))
    }

}

trait TreeMapInstances1 {
  implicit def catsStdEqForTreeMap[K: Order, V: Eq]: Eq[TreeMap[K, V]] =
    new TreeMapEq[K, V]
}

trait TreeMapInstances2 extends TreeMapInstances1 {
  implicit def catsStdMonoidForTreeMap[K: Order, V: Semigroup]: Monoid[TreeMap[K, V]] =
    new TreeMapMonoid[K, V]
}

class TreeMapHash[K, V](implicit V: Hash[V], O: Order[K], K: Hash[K]) extends TreeMapEq[K, V]()(V, O) with Hash[TreeMap[K, V]] {
  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  import scala.util.hashing.MurmurHash3._
  def hash(x: TreeMap[K, V]): Int = {
    var a, b, n = 0
    var c = 1;
    x foreach { case (k, v) =>
      val h = StaticMethods.product2Hash(K.hash(k), V.hash(v))
      a += h
      b ^= h
      if (h != 0) c *= h
      n += 1
    }
    var h = mapSeed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }
}

class TreeMapEq[K, V](implicit V: Eq[V], O: Order[K]) extends Eq[TreeMap[K, V]] {
  def eqv(x: TreeMap[K, V], y: TreeMap[K, V]): Boolean =
    if (x eq y) (O, true)._2
    else x.size == y.size && x.forall { case (k, v1) =>
      y.get(k) match {
        case Some(v2) => V.eqv(v1, v2)
        case None => false
      }
    }
}

class TreeMapCommutativeMonoid[K, V](implicit V: CommutativeSemigroup[V], O: Order[K])
  extends TreeMapMonoid[K, V] with CommutativeMonoid[TreeMap[K, V]]

class TreeMapMonoid[K, V](implicit V: Semigroup[V], O: Order[K]) extends Monoid[TreeMap[K, V]]  {

  def empty: TreeMap[K, V] = TreeMap.empty(O.toOrdering)

  def combine(xs: TreeMap[K, V], ys: TreeMap[K, V]): TreeMap[K, V] =
    if (xs.size <= ys.size) {
      xs.foldLeft(ys) { case (my, (k, x)) =>
        my.updated(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.foldLeft(xs) { case (mx, (k, y)) =>
        mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }

}

object treemap extends TreeMapInstances