// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import web.client.style._

/**
  * Custom CSS for the Seqexec UI
  */
object SeqexecStyles {

  val headerHeight: Int = 33
  val rowHeight: Int = 30
  val overscanRowCount: Int = 10
  val runningRowHeight: Int = 60

  val activeInstrumentLabel: GStyle = GStyle.fromString("SeqexecStyles-activeInstrumentLabel")

  val instrumentTab: GStyle = GStyle.fromString("SeqexecStyles-instrumentTab")

  val instrumentTabLabel: GStyle = GStyle.fromString("SeqexecStyles-instrumentTabLabel")

  val activeInstrumentContent: GStyle = GStyle.fromString("SeqexecStyles-activeInstrumentContent")

  val errorTab: GStyle = GStyle.fromString("SeqexecStyles-errorTab")

  val fieldsNoBottom: GStyle = GStyle.fromString("SeqexecStyles-fieldsNoBottom")

  val headerSideBarStyle: GStyle = GStyle.fromString("SeqexecStyles-headerSidebarStyle")

  val emptyInstrumentTab: GStyle = GStyle.fromString("SeqexecStyles-emptyInstrumentTab")

  val emptyInstrumentTabLogShown: GStyle = GStyle.fromString("SeqexecStyles-emptyInstrumentTabLogShown")

  val emptyInstrumentTabLogHidden: GStyle = GStyle.fromString("SeqexecStyles-emptyInstrumentTabLogHidden")

  val instrumentTabSegment: GStyle = GStyle.fromString("SeqexecStyles-instrumentTabSegment")

  val instrumentTabSegmentLogHidden: GStyle = GStyle.fromString("SeqexecStyles-instrumentTabSegmentLogHidden")

  val instrumentTabSegmentLogShown: GStyle = GStyle.fromString("SeqexecStyles-instrumentTabSegmentLogShown")

  val sequencesArea: GStyle = GStyle.fromString("SeqexecStyles-sequencesArea")

  // Media queries to hide/display items for mobile
  val notInMobile: GStyle = GStyle.fromString("SeqexecStyles-notInMobile")

  val onlyMobile: GStyle = GStyle.fromString("SeqexecStyles-onlyMobile")

  val errorText: GStyle = GStyle.fromString("SeqexecStyles-errorText")

  val noRowsSegment: GStyle = GStyle.fromString("SeqexecStyles-noRowsSegment")

  val logSegment: GStyle = GStyle.fromString("SeqexecStyles-logSegment")

  val logSecondarySegment: GStyle = GStyle.fromString("SeqexecStyles-logSecondarySegment")

  val logControlRow: GStyle = GStyle.fromString("SeqexecStyles-logControlRow")

  val logTableRow: GStyle = GStyle.fromString("SeqexecStyles-logTableRow")

  val selectorFields: GStyle = GStyle.fromString("SeqexecStyles-selectorFields")

  val queueTextColumn: GStyle = GStyle.fromString("SeqexecStyles-queueTextColumn")

  val queueText: GStyle = GStyle.fromString("SeqexecStyles-queueText")

  val daytimeCal: GStyle = GStyle.fromString("SeqexecStyles-daytimeCal")

  val queueIconColumn: GStyle = GStyle.fromString("SeqexecStyles-queueIconColumn")

  val queueListPane: GStyle = GStyle.fromString("SeqexecStyles-queueListPane")

  val labelPointer: GStyle = GStyle.fromString("SeqexecStyles-labelPointer")

  val shorterRow: GStyle = GStyle.fromString("SeqexecStyles-shorterRow")

  val blinking: GStyle = GStyle.fromString("SeqexecStyles-blinking")

  val queueAreaRow: GStyle = GStyle.fromString("SeqexecStyles-queueAreaRow")

  val queueArea: GStyle = GStyle.fromString("SeqexecStyles-queueArea")

  val headerSideBarArea: GStyle = GStyle.fromString("SeqexecStyles-headerSidebarArea")

  val logArea: GStyle = GStyle.fromString("SeqexecStyles-logArea")

  val lowerRow: GStyle = GStyle.fromString("SeqexecStyles-lowerRow")

  val observerField: GStyle = GStyle.fromString("SeqexecStyles-observerField")

  val shorterFields: GStyle = GStyle.fromString("SeqexecStyles-shorterFields")

  val configLabel: GStyle = GStyle.fromString("SeqexecStyles-configLabel")

  val observationProgressRow: GStyle = GStyle.fromString("SeqexecStyles-observationProgressRow")

  val observationProgressBar: GStyle = GStyle.fromString("SeqexecStyles-observationProgressBar")

  val observationBar: GStyle = GStyle.fromString("SeqexecStyles-observationBar")

  val observationLabel: GStyle = GStyle.fromString("SeqexecStyles-observationLabel")

  val guidingCell: GStyle = GStyle.fromString("SeqexecStyles-guidingCell")

  val offsetsBlock: GStyle = GStyle.fromString("SeqexecStyles-offsetsBlock")

  val inlineBlock: GStyle = GStyle.fromString("SeqexecStyles-inlineBlock")

  val configuringRow: GStyle = GStyle.fromString("SeqexecStyles-configuringRow")

  val specialStateLabel: GStyle = GStyle.fromString("SeqexecStyles-specialStateLabel")

  val subsystems: GStyle = GStyle.fromString("SeqexecStyles-subsystems")

  val componentLabel: GStyle = GStyle.fromString("SeqexecStyles-componentLabel")

  val paddedStepRow: GStyle = GStyle.fromString("SeqexecStyles-paddedStepRow")

  val stepRow: GStyle = GStyle.fromString("SeqexecStyles-stepRow")

  val observeConfig: GStyle = GStyle.fromString("SeqexecStyles-observeConfig")

  val headerRowStyle: GStyle = GStyle.fromString("SeqexecStyles-headerRowStyle")

  val infoLog: GStyle = GStyle.fromString("SeqexecStyles-infoLog")

  val errorLog: GStyle = GStyle.fromString("SeqexecStyles-errorLog")

  val warningLog: GStyle = GStyle.fromString("SeqexecStyles-warningLog")

  // Row styles taken from sematic ui tables
  val rowPositive: GStyle = GStyle.fromString("SeqexecStyles-rowPositive")

  val rowWarning: GStyle = GStyle.fromString("SeqexecStyles-rowWarning")

  val rowActive: GStyle = GStyle.fromString("SeqexecStyles-rowActive")

  val rowNegative: GStyle = GStyle.fromString("SeqexecStyles-rowNegative")

  val rowError: GStyle = GStyle.fromString("SeqexecStyles-rowError")

  val rowDisabled: GStyle = GStyle.fromString("SeqexecStyles-rowDisabled")

  val rowNone: GStyle = GStyle.Zero

  val stepRowWithBreakpoint: GStyle = GStyle.fromString("SeqexecStyles-stepRowWithBreakpoint")

  val centeredCell: GStyle = GStyle.fromString("SeqexecStyles-centeredCell")

  val settingsCellHeader: GStyle = GStyle.fromString("SeqexecStyles-settingsCellHeader")

  val tableHeaderIcons: GStyle = GStyle.fromString("SeqexecStyles-tableHeaderIcons")

  val stepsListPane: GStyle = GStyle.fromString("SeqexecStyles-stepsListPane")

  val stepsListPaneWithControls: GStyle = GStyle.fromString("SeqexecStyles-stepsListPaneWithControls")

  val buttonsRow: GStyle = GStyle.fromString("SeqexecStyles-buttonsRow")

  val gutterCell: GStyle = GStyle.fromString("SeqexecStyles-gutterCell")

  val controlCell: GStyle = GStyle.fromString("SeqexecStyles-controlCell")

  val breakPointHandle: GStyle = GStyle.fromString("SeqexecStyles-breakPointHandle")

  val skipHandleHeight: Int = 13

  val skipHandle: GStyle = GStyle.fromString("SeqexecStyles-skipHandle")

  val runningIconCell: GStyle = GStyle.fromString("SeqexecStyles-runningIconCell")

  val errorCell: GStyle = GStyle.fromString("SeqexecStyles-errorCell")

  val skippedIconCell: GStyle = GStyle.fromString("SeqexecStyles-skippedIconCell")

  val iconCell: GStyle = GStyle.fromString("SeqexecStyles-iconCell")

  val settingsCell: GStyle = GStyle.fromString("SeqexecStyles-settingsCell")

  val logIconRow: GStyle = GStyle.fromString("SeqexecStyles-logIconRow")

  val logIconHeader: GStyle = GStyle.fromString("SeqexecStyles-logIconHeader")

  val selectedIcon: GStyle = GStyle.fromString("SeqexecStyles-selectedIcon")

  val runningIcon: GStyle = GStyle.fromString("SeqexecStyles-runningIcon")

  val breakPointOnIcon: GStyle = GStyle.fromString("SeqexecStyles-breakPointOnIcon")

  val breakPointOffIcon: GStyle = GStyle.fromString("SeqexecStyles-breakPointOffIcon")

  val clipboardIconDiv: GStyle = GStyle.fromString("SeqexecStyles-clipboardIconDiv")

  val clipboardIconHeader: GStyle = GStyle.fromString("SeqexecStyles-clipboardIconHeader")

  val tableHeader: GStyle = GStyle.fromString("SeqexecStyles-tableHeader")

  val controlCellRow: GStyle = GStyle.fromString("SeqexecStyles-controlCellRow")

  val settingsCellRow: GStyle = GStyle.fromString("SeqexecStyles-settingsCellRow")
  //   minWidth(35.px).important,
  //   paddingRight(5.px).important,
  //   paddingLeft(0.px).important,
  //   overflow.unset.important,
  //   pointerEvents := "auto"
  // )
  //
  // //
  // // Media query to adjust the width of containers on mobile to the max allowed width
  // val deviceContainer: StyleA = style("ui.container")(
  //   media.only.screen.maxWidth(mobileCut.px)(
  //     width(100.%%).important,
  //     marginLeft(0.px).important,
  //     marginRight(0.px).important
  //   )
  // )
  //
  // val fullCell: StyleS = mixin(
  //   width(100.%%),
  //   height(100.%%)
  // )
  //
  // val queueFullCell: StyleS = mixin(
  //   fullCell,
  //   display.flex,
  //   alignItems.center,
  //   justifyContent.flexStart,
  //   padding(0.px).important
  // )
  //
  // val queueCenterCell: StyleS = mixin(
  //   fullCell,
  //   display.flex,
  //   alignItems.center,
  //   justifyContent.center,
  //   padding(0.px).important
  // )
  //
  // val queueTextM: StyleS = mixin(
  //   color(textColor),
  //   &.hover(
  //     color(textColor)
  //   )
  // )
  //
  // val errorIcon: StyleA = style(
  //   queueTextM,
  //   marginBottom(6.px).important
  // )
  //
  //
  // val widerColumn: StyleS = mixin(
  //   paddingLeft(0.5.rem).important,
  //   paddingRight(0.5.rem).important
  // )
  //
  // val normalizedSegment: StyleS = mixin (
  //   borderRadius.unset.important,
  //   boxShadow := "unset !important"
  // )
  //
  // val stepsListBody: StyleA = style() // Marker css
  // val stepRunning: StyleA = style() // Marker css
  //
  // val inline: StyleA = style {
  //   display.inline
  // }
  //
  // val offsetGrid: StyleA = style {
  //   marginRight(1.em).important
  // }
  //
  // val noPadding: StyleS = mixin(
  //   padding(0.px).important
  // )
  //
  // val noMargin: StyleS = mixin(
  //   margin(0.px)
  // )
  //
  // val hidden: StyleA = style(
  //   display.none
  // )
  //
  // val tdNoPadding: StyleA = style(
  //   noPadding
  // )
  //
  // val noOpacity: StyleA = style(
  //   opacity(0)
  // )
  //
  // val progressVCentered: StyleA = style("ui.progress.vcentered")(
  //   marginBottom(0.px)
  // )
  //
  // // Common properties for a segment displayed when running
  // val segmentRunningMixin: StyleS = mixin(
  //   backgroundColor(rgba(0, 0, 0, 0.0)).important,
  //   color.inherit,
  //   padding(0.5.em, 0.5.em, 0.5.em, 0.em),
  //   noMargin,
  //   (boxShadow := "none").important
  // )
  //
  // val runningLabel: StyleA = style(
  //   backgroundColor(c"#FFFAF3").important,
  //   color(c"#573A08").important
  // )
  //
  // val smallTextArea: StyleA = style(
  //   fontSize.smaller
  // )
  //
  // val appSegment: StyleS = mixin(
  //   paddingTop(0.5.em).important,
  //   paddingBottom(0.5.em).important
  // )
  //
  // val footerSegment: StyleA = style("ui.footer")(
  //   position.fixed,
  //   bottom(0.px),
  //   width(100.%%),
  //   height(46.px),
  //   marginBottom(0.px),
  //   marginTop(0.px),
  //   backgroundColor(c"#F5F5F5"),
  //   borderRadius.unset
  // )
  //
  // val stepsTable: StyleA = style(
  //   // CSS Dark magic to get the gutter background, see
  //   // http://stackoverflow.com/questions/14628601/can-i-add-background-color-only-for-padding
  //   (backgroundImage := s"linear-gradient(to bottom, rgba(249, 0, 1, 0) 0%, rgba(249, 0, 1, 0) 0%), linear-gradient(to right, rgba(34, 36, 38, 0.15) 0px, rgba(34, 36, 38, 0.00001) ${gutterWidth}px)").important,
  //   backgroundClip.contentBox.paddingBox.important
  // )
  //
  // // Styles for the log table, These styles will make react-virtualized
  // // match the look of SemanticUI tables
  // val logTable: StyleA = style(
  //   fontSize(1.em)
  // )
  //
  // val mobileRowHeight: Int = 15
  //
  // val leftBorderMixin: StyleS = mixin(
  //   borderLeftWidth(1.px),
  //   borderLeftStyle.solid,
  //   borderLeftColor(tableBorderColor)
  // )
  //
  // val bottomBorderMixin: StyleS = mixin(
  //   borderBottomWidth(1.px),
  //   borderBottomStyle.solid,
  //   borderBottomColor(tableBorderColor)
  // )
  //
  // val topBorderMixin: StyleS = mixin(
  //   borderTopWidth(1.px),
  //   borderTopStyle.solid,
  //   borderTopColor(tableBorderColor)
  // )
  //
  // val rightBorderMixin: StyleS = mixin(
  //   borderRightWidth(1.px),
  //   borderRightStyle.solid,
  //   borderRightColor(tableBorderColor)
  // )
  //
  // private val cellPaddingMixin: StyleS = mixin(
  //   paddingLeft(0.7.em),
  //   paddingRight(0.7.em)
  // )
  //
  // // Override styles used by react-virtualized
  // val headerRow: StyleA = style("ReactVirtualized__Table__headerRow")(
  //   fontWeight._700,
  //   display.flex,
  //   flexDirection.row,
  //   alignItems.center
  // )
  //
  // // Override styles used by react-virtualized
  // val headerTruncatedText: StyleA = style("ReactVirtualized__Table__headerTruncatedText")(
  //   paddingLeft(7.px)
  // )
  //
  // val firstHeaderColumn: StyleA = style("ReactVirtualized__Table__headerColumn:first-of-type")(
  //   borderLeft.none,
  //   overflow.visible.important
  // )
  //
  // val firstRowColumn: StyleA = style("ReactVirtualized__Table__rowColumn:first-of-type")(
  //   borderLeft.none
  // )
  //
  // val stepsTableMixin: StyleS = mixin(
  //   fontSize.smaller.important,
  //   textOverflow := "ellipsis",
  //   wordWrap.breakWord,
  //   whiteSpace.nowrap
  // )
  //
  // val rowMixin: StyleS = mixin(
  //   topBorderMixin,
  //   rightBorderMixin
  // )
  //
  // val rightCell: StyleA = style(
  //   display.flex,
  //   alignItems.center,
  //   justifyContent.flexEnd
  // )
  //
  // val leftCell: StyleA = style(
  //   display.flex,
  //   alignItems.center,
  //   justifyContent.flexStart
  // )
  //
  // val tableGrid: StyleA = style("ReactVirtualized__Table__Grid")(
  //   leftBorderMixin,
  //   rightBorderMixin,
  //   bottomBorderMixin,
  //   outline.none
  // )
  //
  // val innerScroll: StyleA = style("ReactVirtualized__Grid__innerScrollContainer")(
  //   bottomBorderMixin
  // )
  //
  // private val mobileRow: StyleS = mixin (
  //   media.only.screen.maxWidth(mobileCut.px)(
  //     paddingRight(2.px),
  //     paddingLeft(2.px)
  //   )
  // )
  //
  // val rowColumn: StyleA = style("ReactVirtualized__Table__rowColumn")(
  //   cellPaddingMixin,
  //   leftBorderMixin,
  //   minWidth(0.px),
  //   display.flex,
  //   alignItems.center,
  //   fontSize.small,
  //   textOverflow := "ellipsis",
  //   whiteSpace.nowrap,
  //   height(100.%%),
  //   mobileRow
  // )
  //
  // val row: StyleA = style("ReactVirtualized__Table__row")(
  //   display.flex,
  //   flexDirection.row,
  //   alignItems.center,
  //   &.hover(
  //     backgroundColor(rgba(0, 0, 0, 0.05)),
  //     color(textColor)
  //   ),
  //   &.firstOfType(
  //     borderTop.none
  //   )
  // )
  //
  // val stepRowMixin: StyleS = mixin(
  //   stepsTableMixin,
  //   backgroundColor.white,
  //   color(textColor)
  // )
  //
  //
  // val statusCellMixin: StyleS = mixin(
  //   alignItems.center,
  //   display.flex,
  //   height(100.%%),
  //   width(100.%%),
  //   minWidth(24.px)
  // )
  //
  // val iconCellMixin: StyleS = mixin(
  //   justifyContent.center,
  //   statusCellMixin
  // )
  //
  //
  // val offsetCellWrapper: StyleA = style(
  //   paddingTop(0.4.em)
  // )
  //
}
