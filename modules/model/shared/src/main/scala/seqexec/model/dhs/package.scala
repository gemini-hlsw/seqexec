// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import shapeless.tag
import shapeless.tag.@@

package dhs {

  // Common data types
  trait ImageFileIdT
  trait DataIdT

}

package object dhs {

  type ImageFileId = String @@ ImageFileIdT
  type DataId      = String @@ DataIdT

  implicit val eqImageFileId: Eq[ImageFileId] = Eq.fromUniversalEquals
  implicit val eqDataId: Eq[DataId]           = Eq.fromUniversalEquals

  implicit val monoidImageFileId: Monoid[ImageFileId] =
    new Monoid[ImageFileId] {
      def empty: ImageFileId = toImageFileId(Monoid[String].empty)
      def combine(x: ImageFileId, y: ImageFileId): ImageFileId =
        toImageFileId(Monoid[String].combine(x, y))
    }
  implicit val monoidDataId: Monoid[DataId] = new Monoid[DataId] {
    def empty: DataId = toDataId(Monoid[String].empty)
    def combine(x: DataId, y: DataId): DataId =
      toDataId(Monoid[String].combine(x, y))
  }

  def toImageFileId(i: String): ImageFileId = tag[ImageFileIdT][String](i)
  def toDataId(i:      String): DataId      = tag[DataIdT][String](i)

}
