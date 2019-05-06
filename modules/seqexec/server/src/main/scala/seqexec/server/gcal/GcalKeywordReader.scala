// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.Applicative
import cats.Eq
import cats.data.Nested
import cats.effect.Sync
import cats.effect.LiftIO
import cats.implicits._
import edu.gemini.seqexec.server.gcal.BinaryOnOff
import seqexec.server.keywords._

trait GcalKeywordReader[F[_]] {
  def diffuser: F[String]
  def filter: F[String]
  def lamp: F[String]
  def shutter: F[String]
}

object DummyGcalKeywordsReader {
  def apply[F[_]: Applicative]: GcalKeywordReader[F] = new GcalKeywordReader[F] {

    def diffuser: F[String] = strDefault[F]

    def filter: F[String] = strDefault[F]

    def lamp: F[String] = strDefault[F]

    def shutter: F[String] = strDefault[F]
  }
}

object GcalKeywordsReaderEpics {

  def apply[F[_]: Sync: LiftIO]: GcalKeywordReader[F] = new GcalKeywordReader[F] {
    val sys = GcalEpics.instance
    implicit val eq: Eq[BinaryOnOff] = Eq.by(_.ordinal())

    def diffuser: F[String] = sys.diffuser.safeValOrDefault.to[F]

    def filter: F[String] = sys.filter.safeValOrDefault.to[F]

    def lamp: F[String] = {
      val noValue = none[String].pure[F]
      val onCheck: BinaryOnOff => Boolean = _ === BinaryOnOff.ON
      val ar   = sys.lampAr.delay.map(_.exists(onCheck)).ifM("Ar".some.pure[F], noValue)
      val cuAr = sys.lampCuAr.delay.map(_.exists(onCheck)).ifM("CuAr".some.pure[F], noValue)
      val ir   = Nested(sys.lampIr.delay).map{
        case BinaryOnOff.ON => "IRhigh"
        case _              => "IRlow"
      }.value
      val qh   = sys.lampQH.delay.map(_.exists(onCheck)).ifM("QH".some.pure[F], noValue)
      val thAr = sys.lampThAr.delay.map(_.exists(onCheck)).ifM("ThAr".some.pure[F], noValue)
      val xe   = sys.lampXe.delay.map(_.exists(onCheck)).ifM("Xe".some.pure[F], noValue)

      ar.orElse(xe).orElse(cuAr).orElse(thAr).orElse(qh).orElse(ir).safeValOrDefault
    }

    def shutter: F[String] = Nested(sys.shutter).map {
      case "OPEN"  => "OPEN"
      case "CLOSE" => "CLOSED"
      case _       => "INDEF"
    }.value.safeValOrDefault.to[F]
  }
}
