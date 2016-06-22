package edu.gemini.seqexec.web.server.http4s.encoder

import edu.gemini.seqexec.model._
import edu.gemini.seqexec.web.common._
import boopickle.Default._

trait BooPicklers {
  import edu.gemini.seqexec.web.common.LogMessage._
  import CliCommand._
  import SeqexecEvent._

  // Decoders, Included here instead of the on the object definitions to avoid
  // a circular dependency on http4s
  implicit val userLoginDecoder      = booOf[UserLoginRequest]
  implicit val userDetailEncoder     = booEncoderOf[UserDetails]
  implicit val logMessageDecoder     = booOf[LogMessage]
  implicit val sequenceEncoder       = booEncoderOf[Sequence]
  // The next one seems redundant but apparently we don't find recursive picklers
  implicit val listSequenceEncoder   = booEncoderOf[List[Sequence]]
  implicit val sequexecQueueEncoder  = booEncoderOf[SeqexecQueue]
  implicit val commandsEncoder       = booEncoderOf[CliCommand]

  /**
    * In most cases http4s will use the limit of a byte buffer but not for websockets
    * This method trims the binary array to be sent on the WS channel
    */
  def trimmedArray(e: SeqexecEvent): Array[Byte] = {
    val byteBuffer = Pickle.intoBytes(e)
    val bytes = new Array[Byte](byteBuffer.limit())
    byteBuffer.get(bytes, 0, byteBuffer.limit)
    bytes
  }
}
