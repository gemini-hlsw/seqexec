// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Order
import cats.data.NonEmptySet
import cats.implicits._
import mouse.boolean._
import seqexec.model.enum.Instrument
import seqexec.server.tcs.TcsController.{InstrumentOffset, Subsystem}
import squants.{Ratio, Time}
import squants.space.Angle
import squants.time.TimeConversions._
import squants.space.AngleConversions._

object TcsSettleTimeCalculator {

  trait SettleTimeCalculator {
    def calc(displacement: Angle): Time
  }

  def constantSettleTime(cnst: Time): SettleTimeCalculator = (_: Angle) => cnst

  // Settle time proportional to displacement
  def linearSettleTime(scale: SettleTimeScale): SettleTimeCalculator = (displacement: Angle) => scale * displacement

  final case class SettleTimeScale(time: Time, angle: Angle) extends Ratio[Time, Angle] {
    override def base: Time = time

    override def counter: Angle = angle

    def times(a: Angle): Time = convertToBase(a)
    def *(a: Angle): Time = times(a)
  }

  // We are using constant values for now. Values are taken from old Seqexec
  val settleTimeCalculators: Map[Subsystem, SettleTimeCalculator] = Map(
    Subsystem.Mount -> constantSettleTime(1.seconds),
    Subsystem.PWFS1 -> constantSettleTime(1.seconds),
    Subsystem.PWFS2 -> constantSettleTime(1.seconds)
  )

  val oiwfsSettleTimeCalculators: Map[Instrument, SettleTimeCalculator] = Map(
    Instrument.GmosN -> constantSettleTime(500.milliseconds),
    Instrument.GmosS -> constantSettleTime(500.milliseconds),
    Instrument.F2 -> constantSettleTime(500.milliseconds),
    Instrument.Nifs -> constantSettleTime(4.seconds),
    Instrument.Niri -> constantSettleTime(4.seconds),
    Instrument.Gnirs -> constantSettleTime(4.seconds)
  )

  def calcDisplacement(startOffset: InstrumentOffset, endOffset: InstrumentOffset): Angle =
    math.sqrt(
      math.pow((endOffset.p - startOffset.p).toArcseconds, 2.0) +
        math.pow((endOffset.q - startOffset.q).toArcseconds, 2.0)
    ).arcseconds

  implicit val timeOrder: Order[Time] = Order.fromLessThan{(a: Time, b: Time) => a < b}

  def calc(startOffset: InstrumentOffset,
           endOffset: InstrumentOffset,
           subsystems: NonEmptySet[Subsystem],
           inst: Instrument): Time = {
    val displacement = calcDisplacement(startOffset, endOffset)
    (subsystems.exists(_ === Subsystem.OIWFS).option(oiwfsSettleTimeCalculators(inst))
      :: subsystems.toList.map(settleTimeCalculators.get)
    ).flattenOption.map(_.calc(displacement)).maximumOption.getOrElse(0.seconds)
  }

}
