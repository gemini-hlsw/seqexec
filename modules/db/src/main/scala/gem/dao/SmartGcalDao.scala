package gem.dao

import gem._
import gem.SmartGcal._
import gem.config._
import gem.enum._

import doobie.imports._

import scalaz._, Scalaz._

object SmartGcalDao {

  def selectF2(k: F2SmartGcalKey, t: SmartGcalType): ConnectionIO[List[Int]] = {
    def byLamp(l: GcalLampType): ConnectionIO[List[Int]] =
      sql"""
        SELECT gcal_id
          FROM smart_f2
         WHERE lamp      = $l :: gcal_lamp_type
           AND disperser = ${k.disperser}
           AND filter    = ${k.filter}
           AND fpu       = ${k.fpu}
      """.query[Int].list

    def byBaseline(b: GcalBaselineType): ConnectionIO[List[Int]] =
      sql"""
        SELECT gcal_id
          FROM smart_f2
         WHERE baseline  = $b :: gcal_baseline_type
           AND disperser = ${k.disperser}
           AND filter    = ${k.filter}
           AND fpu       = ${k.fpu}
      """.query[Int].list

    t.fold(byLamp, byBaseline)
  }

  def select(k: SmartGcalKey, t: SmartGcalType): ConnectionIO[List[GcalConfig]] =
    for {
      ids <- k match {
              case f2: F2SmartGcalKey => selectF2(f2, t)
            }
      gcs <- ids.traverseU { GcalDao.select }.map(_.flatten)
    } yield gcs


  def insert(l: GcalLampType, b: GcalBaselineType, k: SmartGcalKey, g: GcalConfig): ConnectionIO[Int] = {
    def insertSmartF2(gcalId: Int, k: F2SmartGcalKey): ConnectionIO[Int] =
      sql"""
        INSERT INTO smart_f2 (lamp, baseline, disperser, filter, fpu, gcal_id)
             VALUES ($l :: gcal_lamp_type, $b :: gcal_baseline_type, ${k.disperser}, ${k.filter}, ${k.fpu}, $gcalId)
      """.update.run

    for {
      id <- GcalDao.insert(g)
      r  <-
        k match {
          case f2: F2SmartGcalKey => insertSmartF2(id, f2)
        }
    } yield r
  }

  private def lookup(step: Option[Step[InstrumentConfig]], loc: Location.Middle): ConnectionIO[LookupResult] = {
    type SmartContext  = (SmartGcalKey, SmartGcalType, InstrumentConfig)

    // Get the key, type, and instrument config from the step.  We'll need this
    // information to lookup the corresponding GcalConfig.
    val context: ExpansionResult[SmartContext] =
      step.fold(stepNotFound(loc).left[SmartContext]) {
        case SmartGcalStep(inst, sgType) =>
          (inst.smartGcalKey \/> noMappingDefined).map { k => (k, sgType, inst) }
        case _                           =>
          notSmartGcal.left[SmartContext]
      }

    // Find the corresponding smart gcal mapping, if any.
    context.fold(e => e.left[ExpandedSteps].point[ConnectionIO], {
      case (k, t, i) => select(k, t).map {
        case Nil  => noMappingDefined.left[ExpandedSteps]
        case gcal => gcal.map { configs => GcalStep(i, configs) }.right[ExpansionError]
      }
    })
  }

  def lookup(oid: Observation.Id, loc: Location.Middle): ConnectionIO[LookupResult] =
    StepDao.selectOne(oid, loc).flatMap(lookup(_, loc))

  def expand(oid: Observation.Id, loc: Location.Middle): ConnectionIO[LookupResult] = {
    // Find the previous and next location for the smart gcal step that we are
    // replacing.  This is needed to generate locations for the steps that will
    // be inserted.
    def bounds(steps: Location.Middle ==>> Step[InstrumentConfig]): (Location, Location) =
      steps.split(loc) match {
        case (prev, next) => (prev.findMax.map(_._1).widen[Location] | Location.beginning,
                              next.findMin.map(_._1).widen[Location] | Location.end)
      }

    def insert(before: Location, gcal: ExpandedSteps, after: Location): ConnectionIO[Unit] =
      Location.find(gcal.size, before, after).toList.zip(gcal).traverseU { case (l, s) =>
        StepDao.insert(oid, l, s)
      }.void

    for {
      steps <- StepDao.selectAll(oid)
      (locBefore, locAfter) = bounds(steps)
      gcal  <- lookup(steps.lookup(loc), loc)
      _     <- gcal.fold(_ => ().point[ConnectionIO], gs =>
                 for {
                   _ <- StepDao.delete(oid, loc)
                   _ <- insert(locBefore, gs, locAfter)
                 } yield ())
    } yield gcal
  }
}
