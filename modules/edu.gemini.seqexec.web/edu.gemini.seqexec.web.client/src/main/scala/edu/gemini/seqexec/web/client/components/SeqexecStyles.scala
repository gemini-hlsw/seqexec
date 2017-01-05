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
    overflow.auto
  )

  val queueListPane = style {
    maxHeight(13.1.em)
  }

  val searchResultListPane = style {
    maxHeight(10.3.em)
  }

  val stepsListPane = style {
    maxHeight(24.3.em)
  }

  val stepsListBody = style() // Marker css
  val stepRunning = style() // Marker css

  val observeConfig = style {
    backgroundColor.lightcyan
  }

  val inline = style {
    display.inline
  }

  val scrollPaneSegment = style("ui.scroll.pane.segment")(
    padding(0.px),
    marginTop(0.px),
    unsafeChild("> .ui.table")(
      border(0.px),
      borderSpacing(0.px)
    )
  )

  val hidden = style(
    display.none
  )

  val tdNoPadding = style(
    padding(0.px).important
  )

  val segmentLittlePadding = style("ui.segment.running")(
    padding(0.2.em),
    margin(0.px),
    borderLeft.none.important
  )

  val segmentsLittlePadding = style("ui.segments.running")(
    padding(0.px),
    margin(0.px),
    border.none,
    borderRadius(0.px)
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
