package gem

import gem.config._
import gem.enum.StepType

import scalaz._, Scalaz._

sealed abstract class Step[A] extends Product with Serializable {
  def instrument: A
}

object Step {
  final case class Location(loc: List[Int]) {
    override def toString: String =
      loc.mkString("{", ",", "}")
  }

  object Location {
    def apply(is: Int*): Location =
      Location(is.toList)

    private val LocationString = """^\{([0-9]+(?:,[0-9]+)*)\}$""".r

    def parse(s: String): Option[Location] =
      s match {
        case LocationString(ds) => Some(Location(ds.split(',').toList.map(_.toInt)))
        case _                  => None
      }

    def unsafeParse(s: String): Location =
      parse(s).getOrElse(sys.error(s"Could not parse location '$s'"))

    implicit val OrderLocation: Order[Location] = Order.order { (l0, l1) =>
      l0.loc.zipAll(l1.loc, 0, 0).find { case (a, b) =>
        a =/= b
      }.fold[Ordering](Ordering.EQ) { case (a, b) =>
        Ordering.fromInt(a - b)
      }
    }
  }
}

final case class BiasStep   [A](instrument: A)                             extends Step[A]
final case class DarkStep   [A](instrument: A)                             extends Step[A]
final case class GcalStep   [A](instrument: A, gcal:      GcalConfig)      extends Step[A]
final case class ScienceStep[A](instrument: A, telescope: TelescopeConfig) extends Step[A]
final case class SmartStep  [A](instrument: A, smartCal:  SmartCalConfig)  extends Step[A]
