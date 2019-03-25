// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.Dataset

trait DatasetLabelMeta {
  import FormatMeta._

  // Dataset.Label as string
  implicit val DatasetLabelMeta: Meta[Dataset.Label] =
    Dataset.Label.fromString.asMeta

}
object DatasetLabelMeta extends DatasetLabelMeta
