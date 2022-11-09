// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import react.common.style._

/**
 * Custom CSS for the Seqexec UI
 */
object SeqexecStyles {

  val headerHeight: Int             = 33
  val rowHeight: Int                = 30
  val overscanRowCount: Int         = 10
  val runningRowHeight: Int         = 60
  val runningBottomRowHeight: Int   = 60
  val TableBorderWidth: Double      = 1.0
  val TableRightPadding: Int        = 13
  val DefaultScrollBarWidth: Double = 15.0

  val DisabledSubsystem: Css =
    Css("SeqexecStyles-disabledSubsystems")

  val SubsystemsForm: Css =
    Css("SeqexecStyles-subsystemsForm")

  val TabControls: Css =
    Css("SeqexecStyles-tabControls")

  val TabTable: Css =
    Css("SeqexecStyles-tabTable")

  val ConfigTableControls: Css =
    Css("SeqexecStyles-configTableControls")

  val SequencesControl: Css =
    Css("SeqexecStyles-sequencesControl")

  val MainUI: Css =
    Css("SeqexecStyles-mainUI")

  val Footer: Css =
    Css("SeqexecStyles-footer")

  val LoginError: Css =
    Css("SeqexecStyles-loginError")

  val ConfirmLine: Css =
    Css("SeqexecStyles-confirmLine")

  val Toast: Css =
    Css("SeqexecStyles-toast")

  val tableDetailRow: Css =
    Css("SeqexecStyles-tableDetailRow")

  val tableDetailRowWithGutter: Css =
    Css("SeqexecStyles-tableDetailRowWithGutter")

  val expandedRunningRow: Css =
    Css("SeqexecStyles-expandedRunningRow")

  val expandedTopRow: Css =
    Css("SeqexecStyles-expandedTopRow")

  val expandedBottomRow: Css =
    Css("SeqexecStyles-expandedBottomRow")

  val activeGuide: Css =
    Css("SeqexecStyles-activeGuide")

  val LoadButton: Css =
    Css("SeqexecStyles-loadButton")

  val activeInstrumentLabel: Css =
    Css("SeqexecStyles-activeInstrumentLabel")

  val activeResourceLabel: Css =
    Css("SeqexecStyles-activeResourceLabel")

  val resourceLabels: Css =
    Css("SeqexecStyles-resourceLabels")

  val tab: Css = Css("SeqexecStyles-tab")

  val TabLabel: Css = Css("SeqexecStyles-tabLabel")

  val PreviewTab: Css =
    Css("SeqexecStyles-previewTabLabel")

  val LoadedTab: Css =
    Css("SeqexecStyles-loadedTab")

  val TabTitleRow: Css =
    Css("SeqexecStyles-tabTitleRow")

  val ResourceLabels: Css =
    Css("SeqexecStyles-resourceLabels")

  val previewTabLoadButton: Css =
    Css("SeqexecStyles-previewTabLoadButton")

  val resourcesTabLabels: Css =
    Css("SeqexecStyles-resourcesTabLabels")

  val activeTabContent: Css =
    Css("SeqexecStyles-activeTabContent")

  val inactiveTabContent: Css =
    Css("SeqexecStyles-inactiveTabContent")

  val errorTab: Css = Css("SeqexecStyles-errorTab")

  val fieldsNoBottom: Css = Css("SeqexecStyles-fieldsNoBottom")

  val SequenceInfo: Css = Css("SeqexecStyles-sequenceInfo")

  val headerSideBarStyle: Css =
    Css("SeqexecStyles-headerSidebarStyle")

  val emptyInstrumentTab: Css =
    Css("SeqexecStyles-emptyInstrumentTab")

  val emptyInstrumentTabLogShown: Css =
    Css("SeqexecStyles-emptyInstrumentTabLogShown")

  val emptyInstrumentTabLogHidden: Css =
    Css("SeqexecStyles-emptyInstrumentTabLogHidden")

  val tabSegment: Css = Css("SeqexecStyles-tabSegment")

  val SequenceControlForm: Css = Css("SeqexecStyles-sequenceControlForm")

  val SequenceControlButtons: Css = Css("SeqexecStyles-sequenceControlButtons")

  // Sometimes we need to manually add css
  val item: Css = Css("item")

  val ui: Css = Css("ui")

  val header: Css = Css("header")

  // Media queries to hide/display items for mobile
  val notInMobile: Css = Css("SeqexecStyles-notInMobile")

  val onlyMobile: Css = Css("SeqexecStyles-onlyMobile")

  val errorText: Css = Css("SeqexecStyles-errorText")

  val noRowsSegment: Css = Css("SeqexecStyles-noRowsSegment")

  val logSegment: Css = Css("SeqexecStyles-logSegment")

  val logSecondarySegment: Css =
    Css("SeqexecStyles-logSecondarySegment")

  val logControlRow: Css = Css("SeqexecStyles-logControlRow")

  val logTableRow: Css = Css("SeqexecStyles-logTableRow")

  val logTable: Css = Css("SeqexecStyles-logTable")

  val selectorFields: Css = Css("SeqexecStyles-selectorFields")

  val logLevelBox: Css = Css("SeqexecStyles-logLevelBox")

  val queueTextColumn: Css =
    Css("SeqexecStyles-queueTextColumn")

  val queueText: Css = Css("SeqexecStyles-queueText")

  val queueIconColumn: Css =
    Css("SeqexecStyles-queueIconColumn")

  val queueListPane: Css = Css("SeqexecStyles-queueListPane")

  val labelPointer: Css = Css("SeqexecStyles-labelPointer")

  val shorterRow: Css = Css("SeqexecStyles-shorterRow")

  val titleRow: Css = Css("SeqexecStyles-titleRow")

  val blinking: Css = Css("SeqexecStyles-blinking")

  val queueAreaRow: Css = Css("SeqexecStyles-queueAreaRow")

  val queueArea: Css = Css("SeqexecStyles-queueArea")

  val headerSideBarArea: Css =
    Css("SeqexecStyles-headerSidebarArea")

  val logArea: Css = Css("SeqexecStyles-logArea")

  val lowerRow: Css = Css("SeqexecStyles-lowerRow")

  val observerField: Css = Css("SeqexecStyles-observerField")

  val shorterFields: Css = Css("SeqexecStyles-shorterFields")

  val configLabel: Css = Css("SeqexecStyles-configLabel")

  val observationProgressRow: Css =
    Css("SeqexecStyles-observationProgressRow")

  val observationProgressBar: Css =
    Css("SeqexecStyles-observationProgressBar")

  val observationBar: Css = Css("SeqexecStyles-observationBar")

  val observationLabel: Css =
    Css("SeqexecStyles-observationLabel")

  val guidingCell: Css = Css("SeqexecStyles-guidingCell")

  val offsetsBlock: Css = Css("SeqexecStyles-offsetsBlock")

  val offsetsNodLabel: Css = Css("SeqexecStyles-offsetsNodLabel")

  val offsetComponent: Css = Css("SeqexecStyles-offsetComponent")

  val configuringRow: Css = Css("SeqexecStyles-configuringRow")

  val specialStateLabel: Css = Css("SeqexecStyles-specialStateLabel")

  val progressMessage: Css = Css("SeqexecStyles-progressMessage")

  val subsystems: Css = Css("SeqexecStyles-subsystems")

  val componentLabel: Css = Css("SeqexecStyles-componentLabel")

  val paddedStepRow: Css = Css("SeqexecStyles-paddedStepRow")

  val stepRow: Css = Css("SeqexecStyles-stepRow")

  val observeConfig: Css = Css("SeqexecStyles-observeConfig")

  val headerRowStyle: Css = Css("SeqexecStyles-headerRowStyle")

  val infoLog: Css = Css("SeqexecStyles-infoLog")

  val errorLog: Css = Css("SeqexecStyles-errorLog")

  val warningLog: Css = Css("SeqexecStyles-warningLog")

  // Row styles taken from sematic ui tables
  val rowPositive: Css = Css("SeqexecStyles-rowPositive")

  val rowWarning: Css = Css("SeqexecStyles-rowWarning")

  val rowActive: Css = Css("SeqexecStyles-rowActive")

  val rowNegative: Css = Css("SeqexecStyles-rowNegative")

  val rowError: Css = Css("SeqexecStyles-rowError")

  val rowDisabled: Css = Css("SeqexecStyles-rowDisabled")

  val rowDone: Css = Css("SeqexecStyles-rowDone")

  val rowNone: Css = Css.Empty

  val stepRowWithBreakpoint: Css =
    Css("SeqexecStyles-stepRowWithBreakpoint")

  val stepDoneWithBreakpoint: Css =
    Css("SeqexecStyles-stepDoneWithBreakpoint")

  val stepRowWithBreakpointHover: Css =
    Css("SeqexecStyles-stepRowWithBreakpointHover")

  val stepRowWithBreakpointAndControl: Css =
    Css("SeqexecStyles-stepRowWithBreakpointAndControl")

  val stepDoneWithBreakpointAndControl: Css =
    Css("SeqexecStyles-stepDoneWithBreakpointAndControl")

  val centeredCell: Css = Css("SeqexecStyles-centeredCell")

  val fullCell: Css = Css("SeqexecStyles-fullCell")

  val tableHeaderIcons: Css =
    Css("SeqexecStyles-tableHeaderIcons")

  val buttonsRow: Css = Css("SeqexecStyles-buttonsRow")

  val gutterCell: Css = Css("SeqexecStyles-gutterCell")

  val controlCell: Css = Css("SeqexecStyles-controlCell")

  val breakPointHandleOn: Css =
    Css("SeqexecStyles-breakPointHandleOn")

  val breakPointHandleOff: Css =
    Css("SeqexecStyles-breakPointHandleOff")

  val skipHandleHeight: Int = 13

  val skipHandle: Css = Css("SeqexecStyles-skipHandle")

  val runningIconCell: Css =
    Css("SeqexecStyles-runningIconCell")

  val completedIconCell: Css =
    Css("SeqexecStyles-completedIconCell")

  val errorCell: Css = Css("SeqexecStyles-errorCell")

  val skippedIconCell: Css =
    Css("SeqexecStyles-skippedIconCell")

  val iconCell: Css = Css("SeqexecStyles-iconCell")

  val settingsCell: Css = Css("SeqexecStyles-settingsCell")

  val logIconRow: Css = Css("SeqexecStyles-logIconRow")

  val logIconHeader: Css = Css("SeqexecStyles-logIconHeader")

  val selectedIcon: Css = Css("SeqexecStyles-selectedIcon")

  val runningIcon: Css = Css("SeqexecStyles-runningIcon")

  val errorIcon: Css = Css("SeqexecStyles-errorIcon")

  val completedIcon: Css = Css("SeqexecStyles-completedIcon")

  val breakPointOnIcon: Css =
    Css("SeqexecStyles-breakPointOnIcon")

  val breakPointOffIcon: Css =
    Css("SeqexecStyles-breakPointOffIcon")

  val clipboardIconDiv: Css =
    Css("SeqexecStyles-clipboardIconDiv")

  val clipboardIconHeader: Css =
    Css("SeqexecStyles-clipboardIconHeader")

  val tableHeader: Css = Css("SeqexecStyles-tableHeader")

  val controlCellRow: Css = Css("SeqexecStyles-controlCellRow")

  val settingsCellRow: Css =
    Css("SeqexecStyles-settingsCellRow")

  val labelAsButton: Css = Css("SeqexecStyles-labelAsButton")

  val calTableBorder: Css = Css("SeqexecStyles-calTableBorder")

  val calRowBackground: Css =
    Css("SeqexecStyles-calRowBackground")

  val autoMargin: Css = Css("SeqexecStyles-autoMargin")

  val deletedRow: Css = Css("SeqexecStyles-deletedRow")

  val noselect: Css = Css("SeqexecStyles-noselect")

  val draggedRowHelper: Css =
    Css("SeqexecStyles-draggedRowHelper")

  val draggableRow: Css =
    Css("SeqexecStyles-draggableRow")

  val filterPane: Css =
    Css("SeqexecStyles-filterPane")

  val filterActiveButton: Css =
    Css("SeqexecStyles-filterActiveButton")

  val dropOnTab: Css =
    Css("SeqexecStyles-dropOnTab")

  val runFrom: Css =
    Css("SeqexecStyles-runFrom")

  val defaultCursor: Css =
    Css("SeqexecStyles-defaultCursor")

  val dividedProgress: Css =
    Css("SeqexecStyles-dividedProgress")

  val dividedProgressSectionLeft: Css =
    Css("SeqexecStyles-dividedProgressSectionLeft")

  val dividedProgressSectionMiddle: Css =
    Css("SeqexecStyles-dividedProgressSectionMiddle")

  val dividedProgressSectionRight: Css =
    Css("SeqexecStyles-dividedProgressSectionRight")

  val dividedProgressBar: Css =
    Css("SeqexecStyles-dividedProgressBar")

  val dividedProgressBarLeft: Css =
    Css("SeqexecStyles-dividedProgressBarLeft")

  val dividedProgressBarMiddle: Css =
    Css("SeqexecStyles-dividedProgressBarMiddle")

  val dividedProgressBarRight: Css =
    Css("SeqexecStyles-dividedProgressBarRight")

  val nodAndShuffleDetailRow: Css =
    Css("SeqexecStyles-nodAndShuffleDetailRow")

  val nodAndShuffleControls: Css =
    Css("SeqexecStyles-nodAndShuffleControls")
}
