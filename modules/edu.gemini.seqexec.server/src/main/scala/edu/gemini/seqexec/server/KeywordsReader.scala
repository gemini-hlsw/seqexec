package edu.gemini.seqexec.server

/**
  * Created by jluhrs on 3/13/17.
  */
object KeywordsReader {
  implicit def fromAnyValue[A](v: A): SeqAction[Option[A]] = SeqAction(Some(v))
  implicit def fromOption[A](v: Option[A]): SeqAction[Option[A]] = SeqAction(v)
}
