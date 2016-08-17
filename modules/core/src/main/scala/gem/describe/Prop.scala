package gem
package describe

import scalaz._
import Scalaz._

/** A property of a larger containing type A.  The property itself is
  * an existential type.  Provides a lens into that property and an equality
  * definition. */
trait Prop[A] { self =>
  type B

  def eq: Equal[B]
  def lens: A @> B

  def meta: Metadata[B]

  /** Determines whether the property value is the same across two instances. */
  def propEqual(a0: A, a1: A): Boolean = eq.equal(lens.get(a0), lens.get(a1))

  /** Invariant map. */
  def xmap[T](f: A => T, g: T => A): Prop[T] { type B = self.B } =
    new Prop[T] {
      type B = self.B

      val eq   = self.eq
      val lens = self.lens.xmapA(f)(g)

      val meta = self.meta
    }

  def compose[T](ta: T @> A): Prop[T] { type B = self.B } =
    new Prop[T] {
      type B = self.B

      val eq   = self.eq
      val lens = self.lens compose ta

      val meta = self.meta
    }
}

object Prop {
  type Aux[A, B0] = Prop[A] { type B = B0 }
}