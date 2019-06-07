// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao.meta

import doobie._
import gsp.math.Index
import gsp.math.syntax.prism._

trait IndexMeta {

  // Index has a DISTINCT type due to its check constraint so we
  // need a fine-grained mapping here to satisfy the query checker.
  implicit val IndexMeta: Meta[Index] =
    Distinct.short("id_index").timap(Index.fromShort.unsafeGet)(_.toShort)

}
object IndexMeta extends IndexMeta

