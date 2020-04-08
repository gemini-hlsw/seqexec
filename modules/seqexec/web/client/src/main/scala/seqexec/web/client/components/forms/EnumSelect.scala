// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.forms

import cats.implicits._
import cats.Show
import gem.util.Enumerated
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.modules.dropdown._
import scala.scalajs.js.JSConverters._
import react.semanticui.collections.form.FormField

/**
  * Produces a dropdown menu, similar to a combobox
  */
final case class EnumSelect[A: Enumerated: Show](
  label:       String,
  value:       Option[A],
  placeholder: String,
  disabled:    Boolean,
  onChange:    A => Callback = (_: A) => Callback.empty
) extends ReactProps {
  @inline def render: VdomElement =
    EnumSelect.component(implicitly[Enumerated[A]], implicitly[Show[A]])(this)
}

object EnumSelect {
  type Props[A] = EnumSelect[A]

  protected def component[A: Enumerated: Show] =
    ScalaComponent
      .builder[Props[A]]("EnumSelect")
      .stateless
      .render_P { p =>
        val enum = implicitly[Enumerated[A]]

        FormField(
          <.label(p.label),
          Dropdown(
            placeholder = p.placeholder,
            fluid       = true,
            selection   = true,
            disabled    = p.disabled,
            value       = p.value.map(i => enum.tag(i)).orUndefined,
            options = enum.all.map(i =>
              DropdownItem(
                text     = i.show,
                value    = enum.tag(i),
                onClickE = (_: ReactMouseEvent, ep: DropdownItem.DropdownItemProps) =>
                  ep.value.toOption
                    .flatMap(v => enum.fromTag(v.asInstanceOf[String]))
                    .map(v => p.onChange(v))
                    .getOrEmpty
              )
            )
          )
        )
      }
      .build
}
