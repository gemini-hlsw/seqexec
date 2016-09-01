package gem
package describe

import scala.language.implicitConversions
import scalaz._
import Scalaz._

/** A typeclass for things that can be described by a list of `Prop`. */
trait Describe[A] { self =>

  /** All properties of A. */
  def props: List[Prop[A]]

  /** Default value for A. */
  def default: A

  /** Compares two values for equality based upon their values of the
    * properties. */
  def equal(a0: A, a1: A): Boolean =
    props.forall(_.propEqual(a0, a1))

  /** Returns a function that transforms `a0` into `a1`. */
  def diff(a0: A, a1: A): A => A =
    props.foldLeft[A => A](identity) { (f, p) =>
      if (p.propEqual(a0, a1)) f else f.andThen(p.lens.set(_, p.lens.get(a1)))
    }

  /** Invariant map. */
  def xmap[B](f: A => B, g: B => A): Describe[B] =
    new Describe[B] {
      val default: B = f(self.default)

      val props = self.props.map(_.xmap(f, g))

      override def equal(b0: B, b1: B): Boolean =
        self.equal(g(b0), g(b1))

      override def diff(b0: B, b1: B): B => B =
        self.diff(g(b0), g(b1)).map(f).compose(g)
    }
}

object Describe {
  /** Summons an instance if in scope. */
  implicit def apply[A](implicit ev: Describe[A]): Describe[A] = ev

  /** Construct an instance from its properties. */
  def forProps[A](defaultValue: A, ps: Prop[A]*): Describe[A] =
    new Describe[A] {
      val default = defaultValue
      val props   = ps.toList
    }

  implicit def describe2[A, B](implicit da: Describe[A], db: Describe[B]): Describe[(A, B)] =
    new Describe[(A, B)] {
      val default: (A, B) =
        (da.default, db.default)

      val props: List[Prop[(A, B)]] =
        da.props.map(_ compose LensFamily.firstLens[A, B]) ++
        db.props.map(_ compose LensFamily.secondLens[A, B])
    }

  implicit def describe3[A: Describe, B: Describe, C: Describe]: Describe[(A, B, C)] =
    describe2[A, (B, C)].xmap[(A, B, C)](
      t => (t._1, t._2._1, t._2._2),  // case (a, (b, c)) => (a, b, c)
      t => (t._1, (t._2, t._3))       // case (a, b, c)   => (a, (b, c))
    )

  implicit def describe4[A: Describe, B: Describe, C: Describe, D: Describe]: Describe[(A, B, C, D)] =
    describe2[A, (B, C, D)].xmap[(A, B, C, D)](
      t => (t._1, t._2._1, t._2._2, t._2._3),  // case (a, (b, c, d)) => (a, b, c, d)
      t => (t._1, (t._2, t._3, t._4))          // case (a, b, c, d)   => (a, (b, c, d))
    )

  import shapeless._

  def hlistHeadLens[A, B <: HList]: (A :: B) @> A = Lens.lensu((a, b) => b :: a.tail, _.head)
  def hlistTailLens[A, B <: HList]: (A :: B) @> B = Lens.lensu((a, b) => a.head :: b, _.tail)

  implicit val HNilDescribe: Describe[HNil] =
    Describe.forProps(HNil)

  implicit def HConsDescribe[A, B <: HList](implicit da: Describe[A], db: Describe[B]): Describe[A :: B] =
    new Describe[A :: B] {
      val default = da.default :: db.default
      val props =
        da.props.map(_ compose hlistHeadLens[A, B]) ++
        db.props.map(_ compose hlistTailLens[A, B])
    }

}


