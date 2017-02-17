import ctl._
import io._

object docker {

  case class Network(hash: String)
  case class Image(hash: String)
  case class Container(hash: String)

  def findNetwork(name: String): CtlIO[Option[Network]] =
    shell("docker", "network", "ls", "-q", "--filter", s"name=$name").require {
      case Output(0, Nil)     => None
      case Output(0, List(h)) => Some(Network(h))
    }

  def createNetwork(name: String): CtlIO[Network] =
    shell("docker", "network", "create", name).require {
      case Output(0, List(h)) => Network(h)
    }

  def findImage(name: String, version: String): CtlIO[Option[Image]] =
    shell("docker", "images", "-q", s"$name:$version").require {
      case Output(0, Nil)     => None
      case Output(0, List(h)) => Some(Image(h))
    }

  // find *running* containers
  def findContainersWithLabel(label: String): CtlIO[List[Container]] =
    shell("docker", "ps", "-q", "--filter", s"label=$label").require {
      case Output(0, hs) => hs.map(Container)
    }

  def allContainerNames: CtlIO[List[String]] =
    shell("docker", "ps", "-a", "--format", "{{.Names}}").require {
      case Output(o, ss) => ss
    }

}
