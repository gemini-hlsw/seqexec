package gem

/** 
 * Type class for enumerated discriminants like program types and observe classes.
 * all.map(tag).traverse(fromTag) == Some(all)
 */
trait Enumerated[A] {
  def all: List[A]
  def tag(a: A): String
  def fromTag(s: String): Option[A] = all.find(tag(_) == s)
  def unsafeFromTag(tag: String): A = fromTag(tag).getOrElse(sys.error("Invalid tag: " + tag))
}

object Enumerated {
  def apply[A](implicit ev: Enumerated[A]): Enumerated[A] = ev
}
