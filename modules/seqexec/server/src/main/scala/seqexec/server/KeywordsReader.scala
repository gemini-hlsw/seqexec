// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

/**
  * Created by jluhrs on 3/13/17.
  */
object KeywordsReader {
  implicit def fromAnyValue[A](v: A): SeqAction[Option[A]] = SeqAction(Some(v))
  implicit def fromOption[A](v: Option[A]): SeqAction[Option[A]] = SeqAction(v)
}
