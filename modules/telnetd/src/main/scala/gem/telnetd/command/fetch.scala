// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package telnetd
package command

import cats.implicits._
import com.monovore.decline.{ Command => _, _ }
import tuco._
import Tuco._
import tuco.shell._

/** Command to fetch an observation or program from an OCS2 ODB.
  */
object fetch {

  val host: Opts[String] =
    Opts.option[String](
      help    = s"ODB hostname (default localhost)",
      metavar = "host",
      short   = "h",
      long    = "host"
    ).withDefault("localhost")

  val oid: Opts[Observation.Id] =
    Opts.argument[String](
      metavar = "obs-id"
    ).mapValidated { s =>
      Observation.Id.fromString(s).toValidNel(s"Could not parse '$s' as an observation id")
    }

  val pid: Opts[Program.Id] =
    Opts.argument[String](
      metavar = "prog-id"
    ).mapValidated { s =>
      ProgramId.fromString.getOption(s).toValidNel(s"Could not parse '$s' as a program id")
    }

  val obsCommand: GemCommand =
    Command(
      "fetch-obs", "Fetch an observation from an OCS2 ODB and store it.",
      (host, oid).mapN { (h: String, id: Observation.Id) => (d: GemState) => {
        for {
          r <- d.ocs2.importObservation(h, id)
          _ <- writeLn(r.fold(m => s"Failed to import ${id.format}: $m", _ => s"Imported ${id.format}"))
        } yield d
      }}
    ).zoom(Session.data[GemState])

  val progCommand: GemCommand =
    Command(
      "fetch-prog", "Fetch a program from an OCS2 ODB and store it.",
      (host, pid).mapN { (h: String, id: Program.Id) => (d: GemState) => {
        for {
          r <- d.ocs2.importProgram(h, id)
          _ <- writeLn(r.fold(
                 m => s"Failed to import ${Program.Id.fromString.reverseGet(id)}: $m",
                 _ => s"Imported ${Program.Id.fromString.reverseGet(id)}"
               ))
        } yield d
      }}
    ).zoom(Session.data[GemState])
}
