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

  val previewTabLabel: GStyle = GStyle.fromString("SeqexecStyles-previewTabLabel")

  val previewTabId: GStyle = GStyle.fromString("SeqexecStyles-previewTabId")

  val previewTabLoadButton: GStyle = GStyle.fromString("SeqexecStyles-previewTabLoadButton")

  val activeInstrumentContent: GStyle = GStyle.fromString("SeqexecStyles-activeInstrumentContent")

  val inactiveInstrumentContent: GStyle = GStyle.fromString("SeqexecStyles-inactiveInstrumentContent")

  val errorTab: GStyle = GStyle.fromString("SeqexecStyles-errorTab")

  val fieldsNoBottom: GStyle = GStyle.fromString("SeqexecStyles-fieldsNoBottom")

  val headerSideBarStyle: GStyle = GStyle.fromString("SeqexecStyles-headerSidebarStyle")

  val emptyInstrumentTab: GStyle = GStyle.fromString("SeqexecStyles-emptyInstrumentTab")

  val emptyInstrumentTabLogShown: GStyle = GStyle.fromString("SeqexecStyles-emptyInstrumentTabLogShown")

  val emptyInstrumentTabLogHidden: GStyle = GStyle.fromString("SeqexecStyles-emptyInstrumentTabLogHidden")

  val instrumentTabSegment: GStyle = GStyle.fromString("SeqexecStyles-instrumentTabSegment")

  val instrumentTabSegmentLogHidden: GStyle = GStyle.fromString("SeqexecStyles-instrumentTabSegmentLogHidden")

  val instrumentTabSegmentLogShown: GStyle = GStyle.fromString("SeqexecStyles-instrumentTabSegmentLogShown")

  val controlButtons: GStyle = GStyle.fromString("SeqexecStyles-controlButtons")

  val infoOnControl: GStyle = GStyle.fromString("SeqexecStyles-infoOnControl")

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

  val rowDone: GStyle = GStyle.fromString("SeqexecStyles-rowDone")

  val rowNone: GStyle = GStyle.Zero

  val stepRowWithBreakpoint: GStyle = GStyle.fromString("SeqexecStyles-stepRowWithBreakpoint")

  val stepRowWithBreakpointAndControl: GStyle = GStyle.fromString("SeqexecStyles-stepRowWithBreakpointAndControl")

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

  val labelAsButton: GStyle = GStyle.fromString("SeqexecStyles-labelAsButton")
}
