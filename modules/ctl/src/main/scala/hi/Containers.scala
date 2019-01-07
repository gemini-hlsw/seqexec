// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import cats.implicits._
import gem.ctl.free.ctl._
import gem.ctl.low.io._
import gem.ctl.low.docker._

object Containers {

  def createDatabaseContainer(version: String, iPg: Image, n: Network): CtlIO[Container] =
    gosub(s"Creating Postgres container from image ${iPg.hash}") {
      for {
        r <- isRemote // irritating … see below
        c <- docker("run",
                  "--detach",
                 s"--net=${n.name}",
                  "--name", s"$version-P",
                  "--label", s"gem.version=$version",
                  "--label", s"gem.role=db",
                  "--health-cmd", if (r) "\"psql -U postgres -d gem -c 'select 1'\""
                                  else     "psql -U postgres -d gem -c 'select 1'",
                  "--health-interval", "10s",
                  "--health-retries", "2",
                  "--health-timeout", "2s",
                  iPg.hash
             ).require { case Output(0, List(s)) => Container(s) }
        _ <- info(s"Container name is $version-P")
        _ <- info(s"Container hash is ${c.hash}.")
      } yield c
    }

  def createGemContainer(version: String, iGem: Image, n: Network): CtlIO[Container] =
    gosub(s"Creating Gem container from image ${iGem.hash}") {
      for {
        _  <- isRemote // irritating … see below
        c  <- docker("run",
                "--detach",
                "--tty",
                "--interactive",
               s"--net=${n.name}",
                "--name",  s"$version-G",
                "--label", s"gem.version=$version",
                "--label", s"gem.role=gem",
                // "--health-cmd", if (r) "\"nc -z localhost 6666\""
                //                 else     "nc -z localhost 6666",
                "--publish", s"9090:9090",
                "--publish", s"9091:9091",
                "--env",     s"GEM_DB_URL=jdbc:postgresql://$version-P/gem",
                iGem.hash
              ).require { case Output(0, List(s)) => Container(s) }
        _ <- info(s"Container name is $version-G")
        _ <- info(s"Container hash is ${c.hash}.")
      } yield c
    }

  def awaitHealthy(k: Container): CtlIO[Unit] =
    containerHealth(k) >>= {
      case "starting" => info( s"Waiting for health check.") *> shell("sleep", "2") *> awaitHealthy(k)
      case "healthy"  => info( s"Container is healthy.")
      case s          => error(s"Health check failed: $s") *> exit(-1)
    }

}
