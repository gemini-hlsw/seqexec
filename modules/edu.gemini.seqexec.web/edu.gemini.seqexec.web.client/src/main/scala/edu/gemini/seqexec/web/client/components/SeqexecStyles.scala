package edu.gemini.seqexec.web.client.components

import scalacss.Defaults._

/**
  * Custom CSS for the Seqexec UI
  */
object SeqexecStyles extends StyleSheet.Inline {
  import dsl._

  val gutterWidth = 25
  val handleContainerWidth = 28
  val iconWidth = 16.5

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

  val progressVCentered = style("ui.progress.vcentered")(
    marginBottom(0.px)
  )

  // Common properties for a segment displayed when running
  val segmentRunningMixin = mixin(
    backgroundColor(rgba(0, 0, 0, 0.0)).important,
    color.inherit,
    padding(0.em),
    margin(0.px),
    (boxShadow := ("none")).important
  )

  // CSS for a segment where a step is running
  val segmentRunning = style("ui.segment.running")(
    segmentRunningMixin,
    borderLeft.none.important,
    alignSelf.center
  )

  // CSS for a segments where a step is running
  val segmentsRunning = style("ui.segments.running")(
    segmentRunningMixin,
    border.none,
    borderRadius(0.px)
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

  val gutterIconVisible = style(
    visibility.visible
  )
  val gutterIconHidden = style(
    visibility.hidden
  )

  val r = style(unsafeRoot("tr")(
    unsafeChild(".SeqexecStyles-breakpointTrOff") {
    &.hover(
      backgroundColor(yellow)
  )}))

  val breakpointTrOn = style(
    height(3.px),
    backgroundColor(c"#A5673F"), // Match semantic UI brown
    borderTop.none.important,
    borderBottom.none,
    &.hover(
      unsafeChild("." + gutterIconHidden.className.value)(
        visibility.visible
      )
    )
  )

  val breakpointTrOff = style(
    height(1.px),
    backgroundColor(lightgray),
    borderTop.none.important,
    borderBottom.none,
    &.hover(
      unsafeChild("." + gutterIconHidden.className.value)(
        visibility.visible
      )
    )
  )

  val breakpointHandleContainer = style(
    position.relative,
    left(((gutterWidth - iconWidth)/2).px),
    top(-29.px),
    height(0.px),
    overflow.visible,
    &.hover(
      unsafeChild("." + gutterIconHidden.className.value)(
        visibility.visible
      )
    )
  )

  val trNoBorder = style(
    borderTop.none.important,
    borderBottom.none.important
  )

  val skipHandleContainer = style(
    position.relative,
    left(((gutterWidth - iconWidth)/2).px),
    top(-11.px),
    height(0.px),
    overflow.visible,
    &.hover(
      unsafeChild("." + gutterIconHidden.className.value)(
        visibility.visible
      )
    )
  )

  val gutterTd = style(
    width(gutterWidth.px),
    maxWidth(gutterWidth.px),
    minWidth(gutterWidth.px),
    borderTop.none.important,
    borderBottom.none.important
  )

  val stepsTable = style(
    //paddingLeft(16.px),
    // CSS Dark magic to get the gutter background, see
    // http://stackoverflow.com/questions/14628601/can-i-add-background-color-only-for-padding
    (backgroundImage := s"linear-gradient(to bottom, rgba(249, 0, 1, 0) 0%, rgba(249, 0, 1, 0) 0%), linear-gradient(to right, rgba(34, 36, 38, 0.15) 0px, rgba(34, 36, 38, 0.00001) ${gutterWidth}px)").important,
    (backgroundClip := "content-box, padding-box").important
  )
}
