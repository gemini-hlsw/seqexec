// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import japgolly.scalajs.react.facade.JsNumber

final case class ColumnRenderArgs[A](
  meta:      ColumnMeta[A],
  index:     Int,
  width:     JsNumber,
  resizable: Boolean
)
