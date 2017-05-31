package edu.gemini.seqexec.web.client.components

import scalacss.DevDefaults._

object SeqexecCSSTypedValues {
  import scalacss.internal._
  import scalacss.internal.ValueT.TypedAttrBase
  // These should be converted to a PR for ScalaCSS

  // Typed backgroundClip
  object backgroundClip extends TypedAttrBase with BackgroundClipDecorationOps {
    override val attr = Attr.real("background-clip", Transform keys CanIUse.backgroundImgOpts)

    override protected def next(v: Value): Accum = new Accum(v)
    final class Accum(v: Value) extends ToAV with BackgroundClipDecorationOps {
      override def av: AV = AV(attr, v)
      override protected def next(v: Value): Accum = new Accum(this.v + " " + v)
    }
  }

  trait BackgroundClipDecorationOps {
    protected def next(v: Value): backgroundClip.Accum
    final def contentBox: backgroundClip.Accum = next(Literal.contentBox)
    final def paddingBox: backgroundClip.Accum = next(Literal.paddingBox)
    final def borderBox: backgroundClip.Accum = next(Literal.borderBox)
  }*/
}

/**
  * Custom CSS for the Seqexec UI
  */
object SeqexecStyles extends scalacss.StyleSheet.Inline {

  import dsl._

  val gutterWidth = 25
  val handleContainerWidth = 28
  val iconWidth = 16.5

  val body: StyleA = style(unsafeRoot("body")(
    backgroundColor(white)
  ))

  val mainContainer: StyleA = style(
    addClassNames("main", "ui", "borderless", "menu", "container")
  )

  val navBar: StyleA = style("navbar")(
    unsafeRoot(".main.ui.borderless.menu.container.placeholder")(
      marginTop(0.px)
    )
  )

  val topLogo: StyleA = style("main.menu .item img.logo")(
    marginRight(1.5.em)
  )

  // Media query to adjust the width of containers on mobile to the max allowed width
  val deviceContainer: StyleA = style("ui.container")(
    media.only.screen.maxWidth(767.px)(
      width(100.%%).important,
      marginLeft(0.px).important,
      marginRight(0.px).important
    )
  )

  val scrollPane: StyleA = style("ui.scroll.pane")(
    overflow.auto
  )

  val queueListPane: StyleA = style {
    maxHeight(13.1.em)
    marginTop(0.px).important
  }

  val searchResultListPane: StyleA = style {
    maxHeight(10.3.em)
  }

  val stepsListPane: StyleA = style {
    maxHeight(24.3.em)
  }

  val stepsListBody: StyleA = style() // Marker css
  val stepRunning: StyleA = style() // Marker css

  val observeConfig: StyleA = style {
    backgroundColor.lightcyan
  }

  val inline: StyleA = style {
    display.inline
  }

  val noPadding: StyleS = mixin(
    padding(0.px).important
  )

  val noMargin: StyleS = mixin(
    margin(0.px)
  )

  val scrollPaneSegment: StyleA = style("ui.scroll.pane.segment")(
    noPadding,
    unsafeChild("> .ui.table")(
      border(0.px),
      borderSpacing(0.px)
    )
  )

  val hidden: StyleA = style(
    display.none
  )

  val tdNoPadding: StyleA = style(
    noPadding
  )

  val errorTab: StyleA = style(
    borderTop(3.px, red, solid).important
  )

  val buttonsRow: StyleA = style(
    marginRight(0.8.rem).important,
    marginLeft(0.8.rem).important
  )

  val progressVCentered: StyleA = style("ui.progress.vcentered")(
    marginBottom(0.px)
  )

  // Common properties for a segment displayed when running
  val segmentRunningMixin: StyleS = mixin(
    backgroundColor(rgba(0, 0, 0, 0.0)).important,
    color.inherit,
    padding(0.5.em, 0.5.em, 0.5.em, 0.em),
    noMargin,
    (boxShadow := "none").important
  )

  // CSS for a segment where a step is running
  val segmentRunning: StyleA = style("ui.segment.running")(
    segmentRunningMixin,
    borderLeft.none.important,
    alignSelf.center
  )

  // CSS for a segments where a step is running
  val segmentsRunning: StyleA = style("ui.segments.running")(
    segmentRunningMixin,
    border.none,
    borderRadius(0.px)
  )

  // Media queries to hide/display items for mobile
  val notInMobile: StyleA = style(
    media.only.screen.maxWidth(767.px)(
      display.none.important
    )
  )
  val onlyMobile: StyleA = style(
    media.only.screen.minWidth(767.px)(
      display.none.important
    )
  )

  val errorText: StyleA = style(
    color.red
  )

  val smallTextArea: StyleA = style(
    fontSize.smaller
  )

  val gutterIconVisible: StyleA = style(
    visibility.visible
  )
  val gutterIconHidden: StyleA = style(
    visibility.hidden
  )

  val breakpointTrOn: StyleA = style(
    height(4.px),
    backgroundColor(c"#A5673F"), // Match semantic UI brown
    borderTop.none.important,
    borderBottom.none
  )

  val breakpointTrOff: StyleA = style(
    height(0.px),
    backgroundColor(lightgray),
    borderTop.none.important,
    borderBottom.none
  )

  val breakpointHandleContainer: StyleA = style(
    position.relative,
    left(((gutterWidth - iconWidth)/2).px),
    top(-27.px),
    height(0.px),
    overflow.visible
  )

  val trNoBorder: StyleA = style(
    borderTop.none.important,
    borderBottom.none.important
  )

  val handleContainerOff: StyleA = style(
    display.none
  )

  val handleContainerOn: StyleA = style(
  )

  val skipHandleContainer: StyleA = style(
    position.relative,
    left(((gutterWidth - iconWidth)/2).px),
    top(-11.px),
    height(0.px),
    overflow.visible
  )

  val gutterTd: StyleA = style(
    width(gutterWidth.px),
    maxWidth(gutterWidth.px),
    minWidth(gutterWidth.px),
    borderTop.none.important,
    borderBottom.none.important,
    borderRight(1.px, solid, rgba(34,36,38,0.1)).important
  )

  val trBreakpoint: StyleA = style()
  // This defines the hover for the gutter
  //SeqexecStyles-trNoBorder:hover > td:first-child {
  val gutterHover: StyleA = style(
    unsafeRoot("tr." + trBreakpoint.htmlClass) (
      &.hover(
        unsafeChild("> td")(
          &.firstChild(
            backgroundColor(rgba(100, 100, 100, 0.1)).important
          )
        )
      )
    )
  )

  val stepsTable: StyleA = style(
    // CSS Dark magic to get the gutter background, see
    // http://stackoverflow.com/questions/14628601/can-i-add-background-color-only-for-padding
    (backgroundImage := s"linear-gradient(to bottom, rgba(249, 0, 1, 0) 0%, rgba(249, 0, 1, 0) 0%), linear-gradient(to right, rgba(34, 36, 38, 0.15) 0px, rgba(34, 36, 38, 0.00001) ${gutterWidth}px)").important,
    SeqexecCSSTypedValues.backgroundClip.contentBox.paddingBox.important
  )
}
