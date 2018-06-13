// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl
package low

import gem.ctl.free.ctl._
import io._

import cats.implicits._

/** Low-level constructors for `CtlIO` operations related to docker. */
object docker {

  final case class Network(hash: String, name: String)
  final case class Image(hash: String)
  final case class Container(hash: String)

  def docker(args: String*): CtlIO[Output] =
    isRemote.map {
      case true  => "/usr/bin/docker"
      case false => "/usr/local/bin/docker"
    } .flatMap(remote(_, args : _*))

  def containerHealth(k: Container): CtlIO[String] = // for now
    isRemote.flatMap { r =>
      docker("inspect", "-f",
        if (r) "'{{ .State.Health.Status }}'"
        else    "{{ .State.Health.Status }}", k.hash).require {
        case Output(0, s :: Nil) => s
      }
    }

  def findNetwork(name: String): CtlIO[Option[Network]] =
    docker("network", "ls", "-q", "--filter", s"name=$name").require {
      case Output(0, Nil)     => None
      case Output(0, List(h)) => Some(Network(h, name))
    }

  def createNetwork(name: String): CtlIO[Network] =
    docker("network", "create", name).require {
      case Output(0, List(h)) => Network(h, name)
    }

  @SuppressWarnings(Array("org.wartremover.warts.TraversableOps")) // .last below
  def pullImage(nameAndVersion: String): CtlIO[Option[Image]] =
    docker("pull", nameAndVersion).require {
      case Output(0, s :: ss) if (s :: ss).last.contains("not found") => None
      case Output(0, _) => Some(nameAndVersion)
    } .flatMap {
      case None                 => none[Image].pure[CtlIO]
      case Some(nameAndVersion) => findImage(nameAndVersion)
    }

  def findImage(nameAndVersion: String): CtlIO[Option[Image]] =
    docker("images", "-q", nameAndVersion).require {
      case Output(0, Nil)     => None
      case Output(0, List(h)) => Some(Image(h))
    }

  // find *running* containers
  def findRunningContainersWithLabel(label: String): CtlIO[List[Container]] =
    docker("ps", "-q", "--filter", s"label=$label").require {
      case Output(0, hs) => hs.map(Container)
    }

  def allContainerNames: CtlIO[List[String]] =
    docker("ps", "-a", "--format", "{{.Names}}").require {
      case Output(_, ss) => ss
    }

  def getLabelValue(label: String, k: Container): CtlIO[String] =
    getLabel(label, k.hash)

  def getImageLabel(label: String, img: Image): CtlIO[String] =
    getLabel(label, img.hash)

  private def getLabel(label: String, obj: String): CtlIO[String] =
    isRemote.flatMap { r =>
      docker("inspect", "--format",
        if (r) s"""'{{ index .Config.Labels "$label"}}'"""
        else    s"""{{ index .Config.Labels "$label"}}""", obj).require {
          case Output(0, s :: Nil) if s.nonEmpty => s
        }
    }

  def ensureImageLabel(label: String, expected: String, img: Image): CtlIO[Unit] =
    getLabel(label, img.hash).flatMap {
      case `expected` => info(s"$label is $expected (as expected)")
      case s          => error(s"$label was $s (expected $expected)") *> exit(-1)
    }

  def stopContainer(k: Container): CtlIO[Unit] =
    docker("stop", k.hash) require {
      case Output(0, s :: Nil) if s === k.hash => ()
    }

  def removeContainer(k: Container): CtlIO[Unit] =
    docker("rm", k.hash) require {
      case Output(0, s :: Nil) if s === k.hash => ()
    }

  def destroyContainer(k: Container): CtlIO[Unit] =
    stopContainer(k) *> removeContainer(k)

  def startContainer(k: Container): CtlIO[Unit] =
    docker("start", k.hash) require {
      case Output(0, s :: Nil) if s === k.hash => ()
    }

  def getContainerName(k: Container): CtlIO[String] =
    isRemote.flatMap { r =>
      docker("inspect", "--format",
        if (r) "'{{ .Name }}'"
        else    "{{ .Name }}", k.hash) require {
        case Output(0, s :: Nil) if (s.startsWith("/")) => s.drop(1)
      }
    }

}
