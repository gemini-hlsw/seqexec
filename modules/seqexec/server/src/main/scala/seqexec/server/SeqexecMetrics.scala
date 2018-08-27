// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.implicits._
import cats.effect.Sync
import gem.enum.Site
import seqexec.model.enum.Instrument
import io.prometheus.client._

final case class SeqexecMetrics private (
  site: Site,
  private val qs: Gauge,  // Amount of items on the list of queues
  private val ss: Counter // Sequences started
)

object SeqexecMetrics {
  private val prefix = "seqexec"

  def build[F[_]: Sync](site: Site, c: CollectorRegistry): F[SeqexecMetrics] =
    Sync[F].delay(
      SeqexecMetrics(
        site,
        qs = Gauge
          .build()
          .name(s"${prefix}_queue_size")
          .help("Queue Size.")
          .labelNames("site")
          .register(c),
        ss = Counter
          .build()
          .name(s"${prefix}_sequence_start")
          .help("Sequence started.")
          .labelNames("site", "instrument")
          .register(c)
      ))

  implicit class SeqexecMetricsOps(val m: SeqexecMetrics) extends AnyVal {

    def queueSize[F[_]: Sync](i: Int): F[SeqexecMetrics] =
      Sync[F].delay {
        m.qs
          .labels(m.site.shortName)
          .set(i.toDouble)
        m
      }

    def startRunning[F[_]: Sync](i: Instrument): F[SeqexecMetrics] =
      Sync[F].delay {
        m.ss
          .labels(m.site.shortName, i.show)
          .inc()
        m
      }

  }
}
