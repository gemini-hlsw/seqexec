/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package gem.ctl
package low

import gem.ctl.free.ctl._
import io._

import scalaz._, Scalaz._

/** Low-level constructors for `CtlIO` operations related to docker. */
object docker {

  case class Network(hash: String)
  case class Image(hash: String)
  case class Container(hash: String)

  def docker(args: String*): CtlIO[Output] =
    remote("/usr/local/bin/docker", args : _*)

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
      case Output(0, List(h)) => Some(Network(h))
    }

  def createNetwork(name: String): CtlIO[Network] =
    docker("network", "create", name).require {
      case Output(0, List(h)) => Network(h)
    }

  def pullImage(nameAndVersion: String): CtlIO[Option[Image]] =
    docker("pull", nameAndVersion).require {
      case Output(0, s :: ss) if (s :: ss).last.contains("not found") => None
      case Output(0, _) => Some(nameAndVersion)
    } .flatMap {
      case None                 => none[Image].point[CtlIO]
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
      case Output(o, ss) => ss
    }

  def getLabelValue(label: String, k: Container): CtlIO[String] =
    isRemote.flatMap { r =>
      docker("inspect", "--format",
        if (r) s"""'{{ index .Config.Labels "$label"}}'"""
        else    s"""{{ index .Config.Labels "$label"}}""", k.hash).require {
          case Output(0, s :: Nil) if s.nonEmpty => s
        }
    }

  def stopContainer(k: Container): CtlIO[Unit] =
    docker("stop", k.hash) require {
      case Output(0, s :: Nil) if s === k.hash => ()
    }

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
