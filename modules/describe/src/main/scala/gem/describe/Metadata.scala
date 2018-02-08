package gem
package describe

import scalaz._
import Scalaz._

/** Metadata describing the values of a property. */
sealed trait Metadata[A] {
  def attrs: Metadata.Attrs
  def show: A => String
  def read: String => String \/ A
}

object Metadata {
  case class Label(parent: Option[Label], name: String)

  object Label {
    def apply(name: String): Label = Label(none, name)

    def apply(parent: Label, name: String): Label = Label(some(parent), name)

    implicit val ShowLabel: Show[Label] = Show.shows { l =>
      l.parent.fold(l.name)(p => s"${p.shows} / ${l.name}")
    }

    implicit val OrderLabel: Order[Label] = Order[String].contramap(_.shows)

    implicit val OrderingLabel: scala.Ordering[Label] = OrderLabel.toScalaOrdering
  }


  sealed trait Access
  object Access {
    case object Engineering extends Access
    case object Science     extends Access
  }

  sealed trait Scope
  object Scope {
    case object Global     extends Scope
    case object SingleStep extends Scope
  }

  case class Attrs(label: Label, access: Access, scope: Scope)
}

/** Metadata for properties with a list of possible values.  The idea is that
  * these will be edited with a combo box widget. */
final case class EnumMetadata[A](attrs: Metadata.Attrs, values: NonEmptyList[A]) extends Metadata[A] {
  override val show: A => String = {
    // case l: LoggableSpType => l.logValue
    case a                 => a.toString
  }

  override val read: String => String \/ A = (s: String) =>
    values.to[List].find(v => show(v) === s) \/> s"${attrs.label} `$s` not recognized"
}

object EnumMetadata {
  // /** Support for creating an EnumMetadata from a Java enum. */
  // def fromJava[A <: java.lang.Enum[A]](attrs: Metadata.Attrs, c: Class[A]): Metadata[A] = {
  //   val values = c.getEnumConstants
  //   EnumMetadata[A](attrs, NonEmptyList.nel(values.head, IList.fromList(values.tail.toList)))
  // }

  def forEnumerated[A](attrs: Metadata.Attrs)(implicit ev: Enumerated[A]): Metadata[A] =
    EnumMetadata(attrs, ev.all.toNel.get)

}




/** Metadata for properties with two values, one that can be interpreted as
  * true and the other false.  These can be edited with check boxes. Note,
  * there is no requirement that "boolean" properties actually have underlying
  * type Boolean. */
final case class BooleanMetadata[A](
    attrs: Metadata.Attrs,
    t: A,
    f: A,
    show: A => String,
    read: String => String \/ A) extends Metadata[A]

object BooleanMetadata {
  /** Support for creating BooleanMetadata for properties with underlying type
    * Boolean. */
  def forBoolean(attrs: Metadata.Attrs): Metadata[Boolean] = {
    BooleanMetadata[Boolean](attrs, true, false, _.toString, {
      case "true"  => true.right
      case "false" => false.right
      case s       => s"${attrs.label.name} value must be `true` or `false`, not `$s".left
    })
  }
}

/** Metadata for properties of arbitrary type that can be represented with a
  * String value.  These properties can be edited with a text field. */
final case class TextMetadata[A](
  attrs: Metadata.Attrs,
  units: Option[String],
  show: A => String,
  read: String => String \/ A) extends Metadata[A]
