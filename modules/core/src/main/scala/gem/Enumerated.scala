package gem

import scalaz._
import Scalaz._

/**
 * Typeclass for an enumerated type with unique string tags.
 * unsafeFromTag . tag = id
 */
trait Enumerated[A] extends Order[A] {
  def all: List[A]
  def tag(a: A): String
  def fromTag(s: String): Option[A] = all.find(tag(_) == s)
  def unsafeFromTag(tag: String): A = fromTag(tag).getOrElse(sys.error("Invalid tag: " + tag))
  def order(a: A, b: A): Ordering = Order[String].order(tag(a), tag(b))
}

object Enumerated {
  def apply[A](implicit ev: Enumerated[A]): Enumerated[A] = ev
}

trait Obsoletable[A] {
  def isActive(a: A): Boolean
  final def isObsolete(a: A): Boolean = !isActive(a)
}


/** Typeclass for things that can be show in a user interface. */
trait Display[A] {
  def name(a:A): String         // short name, for labels
  def elaboration(a:A): Option[String]  // an elaboration on the name, used for computing longname
  def longName(a: A) = name(a) + elaboration(a).fold("")(n => s" ($n)")
}


