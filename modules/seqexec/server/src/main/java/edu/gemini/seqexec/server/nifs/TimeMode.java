/*
 * Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.seqexec.server.nifs;

public enum TimeMode {
  ExposureTime("Exposure time"),
  ReadPeriod("Read period"),
  NrPeriods("Nr Periods");

  private final String name;

  TimeMode(String name) {
      this.name = name;
  }

  @Override
  public String toString() {
      return this.name;
  }

}
