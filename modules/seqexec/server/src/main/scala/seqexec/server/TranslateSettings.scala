// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

final case class TranslateSettings(
  tcsKeywords:    Boolean,
  f2Keywords:     Boolean,
  gwsKeywords:    Boolean,
  gcalKeywords:   Boolean,
  gmosKeywords:   Boolean,
  gnirsKeywords:  Boolean,
  niriKeywords:   Boolean,
  nifsKeywords:   Boolean,
  altairKeywords: Boolean
)
