package gem.dao
package check

import gem._
import gem.enum._
import gem.config._

import doobie.imports._
import doobie.scalatest.imports._
import org.scalatest._

import scalaz._, Scalaz._

/** Trait for tests that check statement syntax and mappings. */
trait Check extends FlatSpec with Matchers with QueryChecker {

  def transactor = DriverManagerTransactor[IOLite](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  /**
   * Some dummy values to pass to statement constructors. The actual values don't matter because
   * the statements are never executed, but they are dereferenced and cannot be null.
   */
  object Dummy {
    val instant       = java.time.Instant.EPOCH
    val duration      = java.time.Duration.ZERO
    val programId     = Program.Id.Arbitrary(None, None, None, "")
    val observationId = Observation.Id(programId, 0)
    val datasetLabel  = Dataset.Label(observationId, 0)
    val dataset       = Dataset(datasetLabel, "", instant)
    val eventType     = EventType.Abort
    val gcalLamp      = GcalContinuum.IrGreyBodyLow.left
    val gcalFilter    = GcalFilter.None
    val gcalDiffuser  = GcalDiffuser.Ir
    val gcalShutter   = GcalShutter.Open
    val gcalConfig    = GcalConfig(gcalLamp, gcalFilter, gcalDiffuser, gcalShutter, duration, 0)
    val user          = User[Nothing]("", "", "", "", false, Map.empty)
    val observation   = Observation[Nothing](observationId, "", None, Nil)
  }

}
