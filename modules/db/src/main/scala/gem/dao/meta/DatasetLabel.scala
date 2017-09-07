// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gem.Dataset

trait DatasetLabelMeta {

  // Dataset.Label as string
  implicit val DatasetLabelMeta: Meta[Dataset.Label] =
    Meta[String].xmap(Dataset.Label.unsafeFromString, _.format)

}
object DatasetLabelMeta extends DatasetLabelMeta
