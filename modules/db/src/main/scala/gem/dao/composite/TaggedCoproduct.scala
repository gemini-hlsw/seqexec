// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.composite

import cats._
import cats.implicits._
import doobie._
import shapeless._
import shapeless.ops.coproduct.Inject

object TaggedCoproduct {

  /**
   * Typeclass witnessing that a coproduct A :+: B :+: ... :+: CNil can be encoded as a pair of
   * tag and product of options  (T, Option[A] :: Option[B] :: ... :: HNil) and can be decoded in
   * cases where the encoding is valid. If the latter type has a Composite instance then the former
   * does as well. This allows us to encode a coproduct as a column vector. The tag is normally
   * redundant but is necessary in when a coproduct element has all Option fields and is thus never
   * read as None.
   */
  sealed trait TaggedCoproductEncoder[T, C <: Coproduct] { outer =>

    /**
     * The product encoding of C, an HList of options, uniquely determined by C. For example
     * A :+: B :+: CNil is encoded as Option[A] :: Option[B] :: HNil.
     */
    type Out <: HList

    /**
     * A value of type Out in which every element is None. So if Out is Option[A] :: Option[B] ::
     * HNil then empty will be None :: None :: HNil.
     */
    def empty: Out

    /** Encode a coproduct C as a tuple of a tag T and hlist of options Out. */
    def encode(c: C): (T, Out)

    /** Inject a value A into C. Users will need to do this if they're mapping an ADT into C. */
    def inj[A: Inject[C, ?]](a: A): C =
      Coproduct(a)

    /** Inject a value A into C and encode. */
    def apply[A: Inject[C, ?]](a: A): (T, Out) =
      encode(inj(a))

    /** Decode a tag T and hlist Out into a C if possible. */
    def decode(t: T, o: Out)(implicit ev: Eq[T]): Option[C]

    /** Decode optimistically, raising an exception on failure. */
    def unsafeDecode(t: T, o: Out)(implicit ev: Eq[T]): C =
      decode(t, o).getOrElse(sys.error(s"invalid tagged coproduct encoding ($t, $o)"))

    /**
     * Extend this encoder by consing a new type/tag onto the front via the little DSL. We can
     * widen the type of T in the process, which proves useful if T is an ADT.
     */
    def :+:[TT >: T, A](t: Tag.Tag[TT, A]): TaggedCoproductEncoder.Aux[TT, A :+: C, Option[A] :: Out] =
      TaggedCoproductEncoder.cons(t.tag, this.widen[TT])

    /**
     * Given Composite[(T, Out)] and Eq[T] we can get Composite[C]. Ultimately this is the purpose
     * of this typeclass.
     */
    def composite(implicit ev: Composite[(T, Out)], eq: Eq[T]): Composite[C] =
      ev.imap((unsafeDecode _).tupled)(encode)

    /**
     * Widen the tag type. This is a no-op but Scala needs some convincing. Observe that you can
     * widen nil and cons trivially by constructing an identical instance with different type
     * arguments, which means the runtime representations must be identical, which means we can just
     * do it as a cast and avoid allocating anything at all.
     */
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def widen[TT >: T]: TaggedCoproductEncoder.Aux[TT, C, Out] =
      this.asInstanceOf[TaggedCoproductEncoder.Aux[TT, C, Out]]

  }
  object TaggedCoproductEncoder {

    /** Universal representation that lets us talk about the Out type member. */
    type Aux[T, C <: Coproduct, O] = TaggedCoproductEncoder[T, C] { type Out = O }

    /**
     * Base case: encoder for an empty coproduct. This is trivial because encode can never be
     * called (CNil is uninhabited) and decode always fails.
     */
    def nil[T]: Aux[T, CNil, HNil] =
      new TaggedCoproductEncoder[T, CNil] {
        type Out = HNil
        override def empty = HNil
        override def encode(c: CNil) = sys.error("unpossible: empty coproduct")
        override def decode(t: T, o: Out)(implicit ev: Eq[T]) = None
      }

    /** Inductive case: extend an encoder by consing a new type/tag onto the front. */
    def cons[T, A, B <: Coproduct](tag: T, te: TaggedCoproductEncoder[T, B]): Aux[T, A :+: B, Option[A] :: te.Out] =
      new TaggedCoproductEncoder[T, A :+: B] {
        type Out = Option[A] :: te.Out
        override def empty = None :: te.empty
        override def encode(c: A :+: B) =
          c match {
            case Inl(a) => (tag, Some(a) :: te.empty)
            case Inr(b) => te.encode(b).map(None :: _)
          }
        override def decode(t: T, o: Out)(implicit ev: Eq[T]) =
          if (t === tag)    o.head .map(Inl(_))
          else te.decode(t, o.tail).map(Inr(_))
      }

  }

  // a little dsl for constructing instances: Tag[Int]("foo") :+: Tag[String]("bar") :+: TNil
  object Tag {
    final case class Tag[+T, A](tag: T)
    def apply[A]: TaggedPartial[A] = new TaggedPartial[A]
    final class TaggedPartial[A] {
      def apply[T](t: T): Tag[T, A] = Tag[T, A](t)
    }
  }
  val TNil: TaggedCoproductEncoder.Aux[Nothing, CNil, HNil] =
    TaggedCoproductEncoder.nil[Nothing]

}
