package edu.gemini.seqexec.web.client.components

import scalacss.Defaults._

/**
  * Custom CSS for the Seqexec UI
  */
object SeqexecStyles extends StyleSheet.Inline {
  import dsl._

  val body = style(unsafeRoot("body")(
    backgroundColor(white)
  ))

  val mainContainer = style(
    addClassNames("main", "ui", "borderless", "menu", "container")
  )

  val navBar = style("navbar")(
    unsafeRoot(".main.ui.borderless.menu.container.placeholder")(
      marginTop(0.px)
    )
  )

  val topLogo = style("main.menu .item img.logo")(
    marginRight(1.5.em)
  )

  // Media query to adjust the width of containers on mobile to the max allowed width
  val deviceContainer = style("ui.container")(
    media.only.screen.maxWidth(767.px)(
      width(100.%%).important,
      marginLeft(0.px).important,
      marginRight(0.px).important
    )
  )

  val scrollPane = style("ui.scroll.pane")(
    overflow.auto,
    maxHeight(17.3.em) // TODO This size may need to be calculated on the fly
  )

  val scrollPaneSegment = style("ui.scroll.pane.attached.segment")(
    padding(0.px),
    unsafeChild("> .ui.table")(
      border(0.px)
    )
  )

  val rowNoPadding = style(
    paddingBottom(0.px).important,
    paddingTop(0.px).important
  )

  // Media queries to hide/display items for mobile
  val notInMobile = style(
    media.only.screen.maxWidth(767.px)(
      display.none.important
    )
  )
  val onlyMobile = style(
    media.only.screen.minWidth(767.px)(
      display.none.important
    )
  )

  val errorText = style(
    color.red
  )

  val smallTextArea = style(
    fontSize.smaller
  )
}
