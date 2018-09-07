/*
 * Copyright 2018 Arsen Ibragimov (ars)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ars.msgpack.structure

import ars.msgpack.structure.implicits._
import ars.precondition.require.Require.Default._
import com.typesafe.scalalogging.Logger
import org.msgpack.core.{MessagePack, MessageUnpacker}
import org.msgpack.value.Value

import scala.util.Try

/** Converts message pack message structure to string representation.
  *
  * @param valueStringifier the value stringifier (must be non-null)
  * @param valueTypeStringifier the value type stringifier (must be non-null)
  *
  * @author Arsen Ibragimov (ars)
  * @since 0.0.1
  */
class MessageStructureStringifier(valueStringifier: ValueStringifier, valueTypeStringifier: ValueTypeStringifier) {

  /**
    * Converts `unpacker` to sequence of structure elements.
    *
    * @param unpacker the unpacker (must be non-null)
    *
    * @return the sequence of structure elements (non-null)
    */
  def toStructure(unpacker: MessageUnpacker): Seq[StructureElement] = {
    requireNotNull(unpacker, "unpacker")
    _toStructure(unpacker)
  }

  /**
    * Converts `unpacker` to human-readable multiline structure string representation.
    *
    * @param unpacker the unpacker (must be non-null)
    * @param linePrefix the line prefix (must be non-null)
    *
    * @return the human-readable multiline structure string representation (non-null)
    */
  def toStructureString(unpacker: MessageUnpacker, linePrefix: String = ""): String = {
    requireNotNull(unpacker, "unpacker")
    requireNotNull(linePrefix, "linePrefix")

    _toStructure(unpacker).zipWithIndex.map {
      case (value, index) => value.toStructureString(f"$linePrefix$index%3d")
    }.mkString("\n")
  }

  /**
    * Tries to convert `bytes` to sequence of structure elements.
    *
    * @param bytes the byte array (must be non-null)
    *
    * @return the sequence of structure elements (non-null)
    */
  def toStructure(bytes: Array[Byte]): Try[Seq[StructureElement]] = {
    requireNotNull(bytes, "unpacker")
    withUnpacker(bytes)(_toStructure)
  }

  /**
    * Tries to convert `bytes` to human-readable multiline structure string representation.
    *
    * @param bytes the byte array (must be non-null)
    * @param linePrefix the line prefix (must be non-null)
    *
    * @return the human-readable multiline structure string representation (non-null)
    */
  def toConsoleString(bytes: Array[Byte], linePrefix: String): Try[String] = {
    requireNotNull(bytes, "unpacker")
    requireNotNull(linePrefix, "linePrefix")

    withUnpacker(bytes)(toStructureString(_, linePrefix))
  }

  /**
    * Tries to convert `bytes` to human-readable multiline structure string representation.
    *
    * @param bytes the byte array (must be non-null)
    *
    * @return the human-readable multiline structure string representation (non-null)
    */
  def toConsoleString(bytes: Array[Byte]): Try[String] = toConsoleString(bytes, "")

  private def _toStructure(unpacker: MessageUnpacker): Seq[StructureElement] = {
    requireNotNull(unpacker, "unpacker")
    unpacker.toValueSeq.map(formatStructureElement)
  }

  private def withUnpacker[T](bytes: Array[Byte])(body: => MessageUnpacker => T): Try[T] = {
    Try { withUnpacker(bytes, body) }
      .recover {
        case exception: Throwable =>
          logger.error("", exception)
          throw exception
      }
  }

  private def withUnpacker[T](bytes: Array[Byte], body: => MessageUnpacker => T): T = {
    val unpacker = MessagePack.newDefaultUnpacker(bytes)
    try {
      body(unpacker)
    } finally {
      unpacker.close()
    }
  }

  private def formatStructureElement(value: Value): StructureElement = {
    StructureElement(
      valueTypeStringifier.formatType(value),
      valueStringifier.formatValue(value)
    )
  }

  private def logger = Logger[MessageStructureStringifier]
}

object MessageStructureStringifier {
  val Default: MessageStructureStringifier = createDefault()

  private def createDefault() = new MessageStructureStringifier(
    new DefaultValueStringifier, new DefaultValueTypeStringifier
  )
}


/** Message structure element.
  *
  * @author Arsen Ibragimov (ars)
  * @since 0.0.1
  */
case class StructureElement(typeName: String, value: String) {
  requireNotNull(typeName, "typeName")
  requireNotNull(value, "value")

  def toStructureString(prefix: String): String = s"$prefix: $typeName -> $value"
  def toStructureString: String = toStructureString("")
}
