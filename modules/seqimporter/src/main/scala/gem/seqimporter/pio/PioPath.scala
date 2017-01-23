package gem.seqimporter.pio

import gem.seqimporter.pio.PioError._

import scala.xml.{Node, NodeSeq}
import scalaz.Scalaz._
import scalaz._

/** PioPath provides a simplistic DSL for working with the OCS3 export format
  * for OCS2 data. The language consists of an operator:
  *
  *  - `\!`  - required element
  *  - `\?`  - optional element
  *  - `\*`  - list of immediate child elements
  *  - `\\*` - list of descendent elements
  *
  *  and a corresponding match string where the initial character is:
  *
  *  - '@' - attribute element
  *  - '#' - `param` element with the given name
  *  - '&' - `paramset` element with the given name
  *  - ``  - (i.e., no special character) child element
  *
  * Example:
  *
  * {{{
  *  (n \?  "instrument" \! "@type")
  * }}}
  *
  * Node with an optional <instrument>` element that, if it exists, has a
  * required "type" attribute.  When decoded, any path that included a `?`
  * but not a `*` will produce an `Option` result.  Any path including a
  * `*` will produce a `List`.
  *
  * If a required element is missing, the result will be `MissingKey`
  * `PioError` when decoded.
  */
object PioPath {

  type PioOptional[A] = OptionT[PioError \/ ?, A]

  object PioOptional {
    def apply[A](a: PioError \/ Option[A]): PioOptional[A] =
      OptionT[PioError \/ ?, A](a)

    def fromOption[A](oa: Option[A]): PioOptional[A] =
      OptionT[PioError \/ ?, A](oa.right)

    def none[A]: PioOptional[A] =
      OptionT.none[PioError \/ ?, A]

    def some[A](a: A): PioOptional[A] =
      OptionT.some[PioError \/ ?, A](a)
  }


  implicit class NodeOps(n: Node) {
    def decode[A](implicit ev: PioDecoder[A]): PioError \/ A =
      ev.decode(n)

    def attr(name: String): Option[Node] =
      (n \ s"@$name").headOption

    def name: Option[String] =
      Some((n \ "@name").text).filterNot(_.isEmpty)

    def value: Option[Node] =
      (n \ "value").headOption orElse (n \ "@value").headOption

    private def filterByName(ns: NodeSeq, name: String): List[Node] =
      ns.filter(_.name.contains(name)).toList

    private def lookupParamValues(ns: NodeSeq, name: String): List[Node] =
      for {
        p <- filterByName(ns, name)
        v <- p.value
      } yield v

    def param(name: String): Option[Node] =
      lookupParamValues(n \ "param", name).headOption

    def params(name: String): List[Node] =
      lookupParamValues(n \ "param", name)

    def deepParams(name: String): List[Node] =
      lookupParamValues(n \\ "param", name)

    def paramset(name: String): Option[Node] =
      paramsets(name).headOption

    def paramsets(name: String): List[Node] =
      filterByName(n \ "paramset", name)

    def deepParamsets(name: String): List[Node] =
      filterByName(n \\ "paramset", name)

    private val root: Required = Required(EmptySearchPath, n.right)

    def \!  (matchString: String): Required  = root \!  matchString
    def \?  (matchString: String): Optional  = root \?  matchString
    def \*  (matchString: String): Listing   = root \*  matchString
    def \\* (matchString: String): Listing   = root \\* matchString
  }

  trait SearchPath {
    protected def append(symbol: String, matchString: String): SearchPath

    def \!  (matchString: String): SearchPath = append("\\!",   matchString)
    def \?  (matchString: String): SearchPath = append("\\?",   matchString)
    def \*  (matchString: String): SearchPath = append("\\*",   matchString)
    def \\* (matchString: String): SearchPath = append("\\\\*", matchString)
  }

  final case object EmptySearchPath extends SearchPath {
    override def toString: String = ""

    protected def append(symbol: String, matchString: String): SearchPath =
      NonEmptySearchPath(s"$symbol $matchString")
  }

  final case class NonEmptySearchPath(override val toString: String) extends SearchPath {
    protected def append(symbol: String, matchString: String): SearchPath =
      NonEmptySearchPath(s"$toString $symbol $matchString")
  }

  trait Lookup {
    def optional: Node => Option[Node]
    def list:     Node => List[Node]
    def deepList: Node => List[Node]
  }

  object Lookup {
    def apply(s: String): Lookup =
      s.splitAt(1) match {
        case ("@", name) => Attr(name)
        case ("#", name) => Param(name)
        case ("&", name) => Paramset(name)
        case _           => Child(s)
      }
  }

  final case class Attr(name: String) extends Lookup {
    def optional: Node => Option[Node] = _.attr(name)
    def list:     Node => List[Node]   = _.child.toList.flatMap(_.attr(name))
    def deepList: Node => List[Node]   = _.descendant.toList.flatMap(_.attr(name))
  }

  final case class Param(name: String) extends Lookup {
    def optional: Node => Option[Node] = _.param(name)
    def list:     Node => List[Node]   = _.params(name)
    def deepList: Node => List[Node]   = _.deepParams(name)
  }

  final case class Paramset(name: String) extends Lookup {
    def optional: Node => Option[Node] = _.paramset(name)
    def list:     Node => List[Node]   = _.paramsets(name)
    def deepList: Node => List[Node]   = _.deepParamsets(name)
  }

  final case class Child(name: String) extends Lookup {
    def optional: Node => Option[Node] = n => (n \ name).headOption
    def list:     Node => List[Node]   = n => (n \ name).toList
    def deepList: Node => List[Node]   = n => (n \\ name).toList
  }

  final case class Required(path: SearchPath, node: PioError \/ Node) {

    def decode[A](implicit ev: PioDecoder[A]): PioError \/ A =
      node.flatMap(ev.decode)

    def \! (matchString: String): Required = {
      val pathʹ = path \! matchString
      val nodeʹ = node.flatMap { n =>
        Lookup(matchString).optional(n) \/> missingKey(pathʹ.toString)
      }
      Required(pathʹ, nodeʹ)
    }

    def \? (matchString: String): Optional = {
      val pathʹ = path \? matchString
      val nodeʹ = PioOptional(node.map(Lookup(matchString).optional))
      Optional(pathʹ, nodeʹ)
    }

    def \* (matchString: String): Listing = {
      val pathʹ = path \* matchString
      val nodeʹ = node.map(Lookup(matchString).list)
      Listing(pathʹ, nodeʹ)
    }

    def \\* (matchString: String): Listing = {
      val pathʹ = path \\* matchString
      val nodeʹ = node.map(Lookup(matchString).deepList)
      Listing(pathʹ, nodeʹ)
    }
  }


  final case class Optional(path: SearchPath, node: PioOptional[Node]) {

    def decode[A](implicit ev: PioDecoder[A]): PioError \/ Option[A] =
      node.flatMapF(ev.decode).run

    def \! (matchString: String): Optional = {
      val pathʹ = path \! matchString
      val nodeʹ = node.flatMapF { n =>
        Lookup(matchString).optional(n) \/> missingKey(pathʹ.toString)
      }
      Optional(pathʹ, nodeʹ)
    }

    def \? (matchString: String): Optional = {
      val pathʹ = path \? matchString
      val nodeʹ = node.flatMap { n =>
        PioOptional.fromOption(Lookup(matchString).optional(n))
      }
      Optional(pathʹ, nodeʹ)
    }

    private def listing(f: Node => List[Node]): PioError \/ List[Node] =
      node.run.map(_.toList.flatMap(f))

    def \* (matchString: String): Listing = {
      val pathʹ = path \* matchString
      val nodeʹ = listing(Lookup(matchString).list)
      Listing(pathʹ, nodeʹ)
    }

    def \\* (matchString: String): Listing = {
      val pathʹ = path \\* matchString
      val nodeʹ = listing(Lookup(matchString).deepList)
      Listing(pathʹ, nodeʹ)
    }
  }


  final case class Listing(path: SearchPath, node: PioError \/ List[Node]) {

    def decode[A](implicit ev: PioDecoder[A]): PioError \/ List[A] =
      node.flatMap { _.traverseU(ev.decode) }

    def \! (matchString: String): Listing = {
      val pathʹ = path \! matchString
      val f     = (n: Node) => Lookup(matchString).optional(n) \/> missingKey(pathʹ.toString)
      val nodeʹ = node.flatMap { _.traverseU(f) }
      Listing(pathʹ, nodeʹ)
    }

    private def listing(f: Node => List[Node]): PioError \/ List[Node] =
      node.map { _.flatMap(f) }

    def \? (matchString: String): Listing = {
      val pathʹ = path \? matchString
      val nodeʹ = listing((n: Node) => Lookup(matchString).optional(n).toList)
      Listing(pathʹ, nodeʹ)
    }

    def \* (matchString: String): Listing = {
      val pathʹ = path \* matchString
      val nodeʹ = listing(Lookup(matchString).list)
      Listing(pathʹ, nodeʹ)
    }

    def \\* (matchString: String): Listing = {
      val pathʹ = path \\* matchString
      val nodeʹ = listing(Lookup(matchString).deepList)
      Listing(pathʹ, nodeʹ)
    }
  }
}
