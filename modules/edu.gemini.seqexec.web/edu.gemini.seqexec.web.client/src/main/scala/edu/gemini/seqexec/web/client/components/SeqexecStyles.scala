// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components

import scalacss.DevDefaults._
import scalacss.internal.Keyframes
import scala.concurrent.duration._

/**
  * Custom CSS for the Seqexec UI
  */
object SeqexecStyles extends scalacss.StyleSheet.Inline {

  import dsl._

  private val gutterWidth = 21
  private val iconWidth = 16.5
  val mobileCut: Int = 767

  val body: StyleA = style(unsafeRoot("body")(
    backgroundColor(white),
    display.flex,
    flexDirection.column
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

  val linkeableRows: StyleA = style(
    unsafeRoot(".ui.table tbody tr td.selectable > a:not(.ui)")(
      paddingTop(0.5.em),
      paddingBottom(0.5.em)
    )
  )

  val logo: StyleA = style(
    height(45.px),
    width(45.px)
  )

  val activeInstrumentLabel: StyleA = style(
    paddingBottom(0.2.em),
    textAlign.center
  )

  val tdNoUpDownPadding: StyleA = style(
    paddingBottom(0.em).important,
    paddingTop(0.em).important
  )

  val activeInstrumentContent: StyleA = style(
    padding(0.6.em, 0.9.em),
    backgroundColor(rgb(243, 244, 245)).important
  )

  val fieldsNoBottom: StyleA = style(
    marginBottom(0.px).important
  )

  // Media query to adjust the width of containers on mobile to the max allowed width
  val deviceContainer: StyleA = style("ui.container")(
    media.only.screen.maxWidth(mobileCut.px)(
      width(100.%%).important,
      marginLeft(0.px).important,
      marginRight(0.px).important
    )
  )

  val scrollPane: StyleA = style("ui.scroll.pane")(
    overflow.auto
  )

  val queueListPane: StyleA = style (
    marginTop(0.px).important,
    media.only.screen.maxWidth(mobileCut.px)(
      maxHeight(10.1.em),
      minHeight(10.1.em)
    ),
    media.only.screen.minWidth(mobileCut.px)(
      maxHeight(15.45.em),
      minHeight(15.45.em)
    )
  )

  val stepsListPane: StyleA = style (
    (height :=! "calc(100% - 1.98em)"),
    marginTop(1.em)
  )

  private val mobileSegment: StyleS = mixin (
    media.only.screen.maxWidth(mobileCut.px)(
      paddingRight(0.px).important,
      paddingLeft(0.px).important
    )
  )

  val instrumentTabSegment: StyleA = style (
    (height :=! "calc(100vh - 48.2em)"),
    mobileSegment,
    media.only.screen.maxWidth(mobileCut.px)(
      minHeight(36.3.em)
    ),
    media.only.screen.minWidth(mobileCut.px)(
      minHeight(25.4.em)
    )
  )

  val sequencesArea: StyleA = style (
    mobileSegment
  )

  val queueArea: StyleA = style (
    mobileSegment
  )

  val logSegment: StyleA = style (
    mobileSegment
  )


  val stepsListPaneWithControls: StyleA = style (
    (height :=! "calc(100% - 4.9em)"),
    marginTop(1.em)
  )

  val stepsListBody: StyleA = style() // Marker css
  val stepRunning: StyleA = style() // Marker css

  val observeConfig: StyleA = style {
    backgroundColor.lightcyan
  }

  val inline: StyleA = style {
    display.inline
  }

  val inlineBlock: StyleA = style {
    display.inlineBlock
  }

  val observerField: StyleA = style {
    paddingRight(0.px).important
  }

  val offsetGrid: StyleA = style {
    marginRight(1.em).important
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

  val shorterRow: StyleA = style(
    marginBottom(-1.em).important
  )

  val emptyInstrumentTab: StyleA = style(
    (height :=! "calc(100vh - 46.2em)"),
    minHeight(25.4.em)
  )

  val instrumentTab: StyleA = style(
    minWidth(20.%%),
    textAlign.center
  )

  val instrumentTabLabel: StyleA = style(
    width(100.%%)
  )

  val lowerRow: StyleA = style(
    marginTop(-1.em).important
  )

  val shorterFields: StyleA = style(
    marginBottom(0.2.em).important
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

  val noOpacity: StyleA = style(
    opacity(0)
  )

  val blink: Keyframes = keyframes(
    50.%% -> noOpacity
  )

  val blinking: StyleA = style(
    animationName(blink),
    animationDuration(1.7.seconds),
    animationIterationCount.infinite,
    animationTimingFunction.cubicBezier(0.5, 0, 1, 1),
    animationDirection.alternate
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
    media.only.screen.maxWidth(mobileCut.px)(
      display.none.important
    )
  )

  val onlyMobile: StyleA = style(
    media.only.screen.minWidth(mobileCut.px)(
      display.none.important
    )
  )

  val errorText: StyleA = style(
    color.red
  )

  val runningLabel: StyleA = style(
    backgroundColor(c"#FFFAF3").important,
    color(c"#573A08").important
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
    left(((gutterWidth - iconWidth) / 2).px),
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

  val logArea: StyleA = style(
    marginBottom(3.em) // Matches the height of the footer
  )

  val footerSegment: StyleA = style("ui.footer")(
    position.fixed,
    bottom(0.px),
    width(100.%%),
    height(46.px),
    marginBottom(0.px),
    marginTop(0.px),
    backgroundColor(c"#F5F5F5"),
    borderRadius.unset
  )

  val stepsTable: StyleA = style(
    // CSS Dark magic to get the gutter background, see
    // http://stackoverflow.com/questions/14628601/can-i-add-background-color-only-for-padding
    (backgroundImage := s"linear-gradient(to bottom, rgba(249, 0, 1, 0) 0%, rgba(249, 0, 1, 0) 0%), linear-gradient(to right, rgba(34, 36, 38, 0.15) 0px, rgba(34, 36, 38, 0.00001) ${gutterWidth}px)").important,
    backgroundClip.contentBox.paddingBox.important
  )

  val daytimeCal: StyleA = style(
    fontWeight.bold,
    fontStyle.italic
  )

  val componentLabel: StyleA = style(
    textOverflow := "ellipsis",
    overflow.hidden,
    wordWrap.breakWord,
    whiteSpace.nowrap
  )

  // Styles for the log table, These styles will make react-virtualized
  // match the look of SemanticUI tables
  val logTable: StyleA = style(
    fontSize(1.em)
  )

  // Border color
  private val tableBorderColor = rgba(34, 36, 38, 0.15)

  val selectorFields: StyleA = style(
    float.right
  )

  val logIconHeader: StyleA = style(
    margin(0.px, 0.px, 10.px, 0.px).important
  )

  val logIconRow: StyleA = style(
    margin(0.px, 0.px, 13.px, 2.px).important
  )

  val overscanRowCount: Int = 10
  val rowHeight: Int = 30
  val mobileRowHeight: Int = 15
  val runningRowHeight: Int = 60
  val headerHeight: Int = 33

  val leftBorderMixin: StyleS = mixin(
    borderLeftWidth(1.px),
    borderLeftStyle.solid,
    borderLeftColor(tableBorderColor)
  )

  val bottomBorderMixin: StyleS = mixin(
    borderBottomWidth(1.px),
    borderBottomStyle.solid,
    borderBottomColor(tableBorderColor)
  )

  val topBorderMixin: StyleS = mixin(
    borderTopWidth(1.px),
    borderTopStyle.solid,
    borderTopColor(tableBorderColor)
  )

  val rightBorderMixin: StyleS = mixin(
    borderRightWidth(1.px),
    borderRightStyle.solid,
    borderRightColor(tableBorderColor)
  )

  private val cellPaddingMixin: StyleS = mixin(
    paddingLeft(0.7.em),
    paddingRight(0.7.em)
  )

  val tableHeader: StyleA = style(
    leftBorderMixin,
    fontWeight.bold,
    color(black),
    backgroundColor(c"#F9FAFB"),
    outline.none,
    minHeight(headerHeight.px),
    height(headerHeight.px),
    paddingLeft(6.px),
    paddingTop(6.px)
  )

  // Override styles used by react-virtualized
  val headerRow: StyleA = style("ReactVirtualized__Table__headerRow")(
    fontWeight._700,
    display.flex,
    flexDirection.row,
    alignItems.center
  )

  val firstHeaderColumn: StyleA = style("ReactVirtualized__Table__headerColumn:first-of-type")(
    borderLeft.none,
    overflow.visible.important
  )

  val firstRowColumn: StyleA = style("ReactVirtualized__Table__rowColumn:first-of-type")(
    borderLeft.none
  )

  val stepsTableMixin: StyleS = mixin(
    fontSize.smaller.important,
    textOverflow := "ellipsis",
    wordWrap.breakWord,
    whiteSpace.nowrap
  )

  val rowMixin: StyleS = mixin(
    leftBorderMixin,
    topBorderMixin,
    rightBorderMixin
  )

  val centeredCell: StyleA = style(
    display.flex,
    alignItems.center,
    justifyContent.center
  )

  val rightCell: StyleA = style(
    display.flex,
    alignItems.center,
    justifyContent.flexEnd
  )

  val leftCell: StyleA = style(
    display.flex,
    alignItems.center,
    justifyContent.flexStart
  )

  val controlCellHeader: StyleA = style(
    paddingBottom((headerHeight/4).px)
  )

  val tableGrid: StyleA = style("ReactVirtualized__Table__Grid")(
    topBorderMixin,
    bottomBorderMixin,
    outline.none
  )

  val innerScroll: StyleA = style("ReactVirtualized__Grid__innerScrollContainer")(
    bottomBorderMixin
  )

  private val mobileRow: StyleS = mixin (
    media.only.screen.maxWidth(mobileCut.px)(
      paddingRight(2.px),
      paddingLeft(2.px)
    )
  )

  // This must be defined before rowColumn
  val controlCellRow: StyleA = style(
    paddingRight(0.px).important,
    paddingLeft(0.px).important,
    overflow.unset.important
  )

  // This must be defined before rowColumn
  val settingsCellRow: StyleA = style(
    paddingRight(0.px).important,
    paddingLeft(0.px).important,
    overflow.unset.important
  )

  val rowColumn: StyleA = style("ReactVirtualized__Table__rowColumn")(
    cellPaddingMixin,
    leftBorderMixin,
    minWidth(0.px),
    display.flex,
    alignItems.center,
    fontSize.small,
    textOverflow := "ellipsis",
    whiteSpace.nowrap,
    height(100.%%),
    mobileRow
  )

  val headerRowStyle: StyleA = style(
    rowMixin,
    backgroundColor(c"#F9FAFB"),
    minHeight(headerHeight.px),
    height(headerHeight.px)
  )

  val stepRowMixin: StyleS = mixin(
    stepsTableMixin,
    backgroundColor.white,
    color(rgba(0, 0, 0, 0.95))
  )

  val stepRow: StyleA = style(
    stepRowMixin,
    overflow.unset.important,
    rowMixin,
    outline.none
  )

  val stepRowWithBreakpoint: StyleA = style(
    stepRowMixin,
    leftBorderMixin,
    rightBorderMixin,
    backgroundImage := "linear-gradient(to right, rgba(34, 36, 38, 0.15) , rgba(34, 36, 38, 0.15) 20px, #a5673f 20px, #a5673f)",
    backgroundSize := "100% 4px",
    backgroundRepeat.noRepeat,
    borderTopColor(tableBorderColor),
    borderTopWidth(1.px),
    borderTopStyle.solid,
    paddingTop(4.px),
    overflow.visible.important,
    outline.none
  )

  val infoLog: StyleA = style(
    rowMixin,
    backgroundColor.white,
    color(rgba(0, 0, 0, 0.95))
  )

  val errorLog: StyleA = style(
    rowMixin,
    backgroundColor(c"#fff6f6").important,
    color(c"#9f3a38").important
  )

  val warningLog: StyleA = style(
    rowMixin,
    backgroundColor(c"#fffaf3").important,
    color(c"#573a08").important
  )

  val controlCell: StyleA = style(
    display.flex,
    justifyContent.spaceAround,
    alignItems.center,
    minWidth(42.3.px)
  )

  val settingsCellHeader: StyleA = style(
    display.flex,
    alignItems.center,
    justifyContent.center,
    paddingLeft(-2.px)
  )

  val settingsCell: StyleA = style(
    display.flex,
    justifyContent.center,
    alignItems.center,
    minWidth(15.px),
    width(100.%%),
    height(100.%%),
    paddingBottom(9.px)
  )

  val statusCellMixin: StyleS = mixin(
    alignContent.center,
    display.flex,
    height(100.%%),
    width(100.%%),
    minWidth(24.px)
  )

  val iconCellMixin: StyleS = mixin(
    justifyContent.center,
    statusCellMixin
  )

  val iconCell: StyleA = style(
    iconCellMixin
  )

  val runningIconCell: StyleA = style(
    iconCellMixin
  )

  val errorCell: StyleA = style(
    iconCellMixin,
    paddingBottom(8.px)
  )

  val skippedIconCell: StyleA = style(
    iconCellMixin,
    paddingRight(10.px)
  )

  val gutterCell: StyleA = style(
    // CSS Dark magic to get the gutter background, see
    // http://stackoverflow.com/questions/14628601/can-i-add-background-color-only-for-padding
    (backgroundImage := s"linear-gradient(to bottom, rgba(249, 0, 1, 0) 0%, rgba(249, 0, 1, 0) 0%), linear-gradient(to right, rgba(34, 36, 38, 0.15) 0px, rgba(34, 36, 38, 0.00001) ${gutterWidth}px)").important,
    backgroundClip.paddingBox,
    backgroundColor(white),
    width(gutterWidth.px),
    minWidth(gutterWidth.px),
    rightBorderMixin,
    position.relative
  )

  val offsetCellWrapper: StyleA = style(
    paddingTop(0.4.em)
  )

  val breakPointOnIcon: StyleA = style(
    position.relative,
    left(3.px),
    top(-5.px)
  )

  val breakPointOffIcon: StyleA = style(
    position.relative,
    left(2.px),
    top(-5.px)
  )

  val breakPointHandle: StyleA = style(
    position.relative,
    left(3.px),
    top(-6.px),
    height(13.px),
    width(13.px),
    cursor.pointer,
    opacity(0.8),
    transition := "all .1s ease-in-out",
    &.hover(
      transform := "scale(1.4)"
    )
  )

  // Row styles taken from sematic ui tables
  val rowDisabled: StyleA = style(
    pointerEvents := "none",
    color(rgba(40, 40, 40, 0.3))
  )

  val rowActive: StyleA = style(
    backgroundColor(c"#E0E0E0"),
    color(rgba(0, 0, 0, 0.87)),
    boxShadow := "0px 0px 0px rgba(0, 0, 0, 0.87) inset",
    backgroundClip.paddingBox
  )

  val rowNegative: StyleA = style(
    backgroundColor(c"#FFF6F6"),
    color(c"#9F3A38"),
    boxShadow := "0px 0px 0px #E0B4B4 inset"
  )

  val rowPositive: StyleA = style(
    backgroundColor(c"#FCFFF5"),
    color(c"#2C662D"),
    boxShadow := "0px 0px 0px #9F3A38 inset"
  )

  val rowError: StyleA = style(
    backgroundColor(c"#FFF6F6"),
    color(c"#9F3A38"),
    boxShadow := "0px 0px 0px #E0B4B4 inset"
  )

  val rowWarning: StyleA = style(
    backgroundColor(c"#FFFAF3"),
    color(c"#573A08"),
    boxShadow := "0px 0px 0px #C9BA9B inset"
  )

  val rowNone: StyleA = style(
  )
  // End row styles

  val configuringRow: StyleA = style(
    display.flex,
    flexGrow(1)
  )

  val subsystems: StyleA = style(
    marginLeft.auto
  )

  private val obsWidth = 200

  val observationProgressRow: StyleA = style(
    display.flex,
    flexGrow(1),
    flexDirection.column,
    width(obsWidth.px)
  )

  val observationProgressBar: StyleA = style(
    margin(0.px).important,
    width(obsWidth.px)
  )

  val observationBar: StyleA = style(
    height(1.em).important
  )

  val observationLabel: StyleA = style(
    textAlign.center,
    width(obsWidth.px)
  )

  val specialStateLabel: StyleA = style(
    display.flex,
    flexGrow(1),
    alignSelf.center,
    textOverflow := "ellipsis",
    overflow.hidden,
    wordWrap.breakWord,
    whiteSpace.nowrap
  )

  val skipHandleHeight: Int = 13

  val skipHandle: StyleA = style(
    position.absolute,
    left(2.1.px),
    height(skipHandleHeight.px),
    width(skipHandleHeight.px),
    cursor.pointer,
    opacity(0.8),
    transition := "all .1s ease-in-out",
    &.hover(
      transform := "scale(1.2)"
    )
  )

  val queueAreaRow: StyleA = style(
    media.only.screen.maxWidth(mobileCut.px)(
      paddingTop(0.px).important,
      paddingBottom(0.px).important
    )
  )

}
