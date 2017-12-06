// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import cats.implicits._
import gem.ctl.free.ctl._
import gem.ctl.low.io._
import gem.ctl.low.docker._

object Postgres {

  def createDatabase(version: String, kPg: Container): CtlIO[Unit] =
    for {
      r <- isRemote // irritating … see below
      b <- docker("exec", kPg.hash,
             "psql", s"--host=$version-P",
                     if (r) "--command='create database gem'"
                     else   "--command=create database gem",
                      "--username=postgres"
           ).require {
             case Output(0, List("CREATE DATABASE")) => true
             case Output(2, _)                       => false
           }
      _ <- if (b) info("Created database.")
           else {
             info(s"Waiting for Postgres to start up.") *>
             shell("sleep", "2") *>
             createDatabase(version, kPg)
           }
    } yield ()

}
