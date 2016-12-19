package gem.dao

import gem.config.{F2SmartGcalKey, GcalConfig, SmartGcalKey}
import gem.enum.{GcalBaselineType, GcalLampType, SmartGcalType}
import doobie.imports._
import scalaz._, Scalaz._

object SmartGcalDao {
  def split(t: SmartGcalType): GcalLampType \/ GcalBaselineType =
    t match {
      case SmartGcalType.Arc           => GcalLampType.Arc.left
      case SmartGcalType.Flat          => GcalLampType.Flat.left
      case SmartGcalType.DayBaseline   => GcalBaselineType.Day.right
      case SmartGcalType.NightBaseline => GcalBaselineType.Night.right
    }

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

    split(t).fold(byLamp, byBaseline)
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

}
