package gem.dao

import gem.config.{F2SmartGcalKey, GcalConfig, SmartGcalKey}
import gem.enum.{GcalBaselineType, GcalLampType}
import doobie.imports._

object SmartGcalDao {
  def insert(l: GcalLampType, b: GcalBaselineType, k: SmartGcalKey, g: GcalConfig): ConnectionIO[Int] = {
    def insertSmartF2(gcalId: Int, k: F2SmartGcalKey): ConnectionIO[Int] =
      sql"""
        INSERT INTO smart_f2 (lamp, baseline, disperser, filter, fpu, gcal_id)
             VALUES ($l, $b, ${k.disperser}, ${k.filter}, ${k.fpu}, $gcalId)
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
