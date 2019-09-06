// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import mouse.boolean._
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

  def apply[F[_]: Sync](sys: GcalEpics[F]): GcalKeywordReader[F] = new GcalKeywordReader[F] {

    def diffuser: F[String] = sys.diffuser.safeValOrDefault

    def filter: F[String] = sys.filter.safeValOrDefault

    def lamp: F[String] = {
      def onCheck(v: BinaryOnOff): Boolean = v === BinaryOnOff.ON

      for {
        ar   <- sys.lampAr.map(onCheck(_).option("Ar"))
        xe   <- sys.lampXe.map(onCheck(_).option("Xe"))
        cuAr <- sys.lampCuAr.map(onCheck(_).option("CuAr"))
        thAr <- sys.lampThAr.map(onCheck(_).option("ThAr"))
        qh   <- sys.lampQH.map(onCheck(_).option("QH"))
        ir   <- sys.lampIr.map {
          case BinaryOnOff.ON => "IRhigh".some
          case _ => "IRlow".some
        }
      } yield ar.orElse(xe).orElse(cuAr).orElse(thAr).orElse(qh).orElse(ir)
    }.safeValOrDefault

    def shutter: F[String] = sys.shutter.map {
      case "OPEN" => "OPEN"
      case "CLOSE" => "CLOSED"
      case _ => "INDEF"
    }.safeValOrDefault
  }
}
