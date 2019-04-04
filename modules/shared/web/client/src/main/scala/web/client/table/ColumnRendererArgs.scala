// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import japgolly.scalajs.react.raw.JsNumber

final case class ColumnRenderArgs[A](meta:      ColumnMeta[A],
                                     index:     Int,
                                     width:     JsNumber,
                                     resizable: Boolean)
