// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.implicits._
import edu.gemini.spModel.config2.{Config, DefaultConfig, ItemEntry, ItemKey}
import seqexec.server.ConfigUtilOps._
import edu.gemini.spModel.gemini.gmos.InstGmosCommon.USE_NS_PROP
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.obscomp.InstConstants.{ARC_OBSERVE_TYPE, BIAS_OBSERVE_TYPE, FLAT_OBSERVE_TYPE, OBSERVE_TYPE_PROP}
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants.GUIDE_WITH_OIWFS_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.{INSTRUMENT_KEY, TELESCOPE_KEY}
import seqexec.model.StepConfig
import seqexec.model.enum.SystemName

/*
 * CleanConfig is a wrapper over Config that allows to override some of the Config parameters. It allows to change some
 * of the Config parameters without mutation.
 * It is used to correct some inconsistencies in the sequences coming from the ODB. It has only one of those corrections
 * now, but it allows for more to be added in the future.
 */
final case class CleanConfig(config: Config, overrides: Map[ItemKey, AnyRef]) {

  def itemValue(k: ItemKey): Option[AnyRef] = overrides.get(k).orElse(Option(config.getItemValue(k)))

  private def sanitizeValue(s: Any): String = s match {
    case x: edu.gemini.shared.util.immutable.Some[_] => s"${x.getValue}"
    case _: edu.gemini.shared.util.immutable.None[_] => "None"
    case _ => s"$s"
  }

  def itemEntries: List[ItemEntry] = config.itemEntries.filter(x => !overrides.contains(x.getKey)).toList ++
    overrides.map{ case (k, v) => new ItemEntry(k, v) }

  // config syntax: cfg.toStepConfig
  def toStepConfig: StepConfig =
    itemEntries.groupBy(_.getKey.getRoot).map {
      case (subsystem, entries) =>
        SystemName.unsafeFromString(subsystem.getName) ->
          entries.map { e =>
            (e.getKey.getPath, sanitizeValue(e.getItemValue))
          }.toMap
    }

}

object CleanConfig {

  implicit val extractItem: ExtractItem[CleanConfig] =
    (a: CleanConfig, key: ItemKey) => a.itemValue(key)

  type ConfigWiper = Config => Map[ItemKey, AnyRef]

  // We want a new one each time this is called as the underlying Config is mutable
  def empty: CleanConfig = apply(new DefaultConfig())

  // GMOS arcs, flats and biases in a N&S sequence have shuffle parameters, even if that is
  // not supported. The shuffling is automatically disabled by setting the useNS flag to false.
  val nsWiper: ConfigWiper = cfg => (
    for {
      useNS <- cfg.extractInstAs[java.lang.Boolean](USE_NS_PROP)
      obsType <- cfg.extractObsAs[String](OBSERVE_TYPE_PROP)
    } yield
      if (useNS && (obsType === ARC_OBSERVE_TYPE || obsType === FLAT_OBSERVE_TYPE || obsType === BIAS_OBSERVE_TYPE))
        Map(INSTRUMENT_KEY / USE_NS_PROP -> (java.lang.Boolean.FALSE:AnyRef))
      else
        Map.empty[ItemKey, AnyRef]
  ).getOrElse(Map.empty[ItemKey, AnyRef])

  // OIWFS must never guide in a GCAL calibration
  val oiCalWiper: ConfigWiper = cfg => (
    for {
      obsType <- cfg.extractObsAs[String](OBSERVE_TYPE_PROP)
      oiGuide <- cfg.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_OIWFS_PROP)
    } yield
      if(oiGuide.isActive && (obsType === ARC_OBSERVE_TYPE || obsType === FLAT_OBSERVE_TYPE))
        Map(TELESCOPE_KEY / GUIDE_WITH_OIWFS_PROP -> StandardGuideOptions.Value.freeze)
      else
        Map.empty[ItemKey, AnyRef]
  ).getOrElse(Map.empty[ItemKey, AnyRef])

  def createCleanConfig(config: Config, l: List[ConfigWiper]): CleanConfig =
    new CleanConfig(config, l.map(_(config)).reduce(_ ++ _))

  // New ConfigWiper must be added to the List
  def apply(config: Config): CleanConfig = createCleanConfig(config, List(nsWiper, oiCalWiper))
}
