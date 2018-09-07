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

import ars.precondition.require.Require.Default._

/** Scala type stringifier.
  *
  * @author Arsen Ibragimov (ars)
  * @since 0.0.1
  */
trait ScalaTypeStringifier {

  val EmptyMapValueString = "{}"

  type Formatter[T] = T => String

  /**
    * Converts [[Boolean]] to human-readable string.
    *
    * @param value the value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatBoolean(value: Boolean): String = value.toString

  /**
    * Converts [[BigInt]] to human-readable string.
    *
    * @param value the value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatBigInt(value: BigInt): String = value.toString()

  /**
    * Converts [[Array]] of bytes to human-readable string.
    *
    * @param value the value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatByteArray(value: Array[Byte]): String = {
    requireNotNull(value, "value")

    val hexString = value.map(_.toHexString).mkString("_")
    s"0x$hexString"
  }

  /**
    * Converts [[List]] to human-readable string.
    *
    * @param value the value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatList[T](formatter: Formatter[T])(value: List[T]): String = {
    requireNotNull(formatter, "formatter")
    requireNotNull(value, "value")

    val elemsString = value.map(formatter).mkString(", ")
    s"[$elemsString]"
  }

  /**
    * Converts pair to human-readable string.
    *
    * @param keyFormatter the key formatter (must be non-null)
    * @param valueFormatter the value formatter (must be non-null)
    * @param tuple the tuple (must be non-null)
    *
    * @tparam K the key type
    * @tparam V the key type
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatTuple[K, V](keyFormatter: Formatter[K], valueFormatter: Formatter[V])(tuple: (K, V)): String = {
    requireNotNull(keyFormatter, "keyFormatter")
    requireNotNull(valueFormatter, "valueFormatter")

    requireNotNull(tuple, "tuple")
    requireNotNull(tuple._1, "tuple._1")
    requireNotNull(tuple._2, "tuple._2")

    val (k, v) = tuple
    val keyString = keyFormatter(k)
    val valueString = valueFormatter(v)
    s"$keyString -> $valueString"
  }

  /**
    * Converts map to human-readable string.
    *
    * @param formatter the tuple formatter (must be non-null)
    * @param map the map (must be non-null)
    *
    * @tparam K the key type
    * @tparam V the key type
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatMap[K, V](formatter: Formatter[(K, V)])(map: Map[K, V]): String = {
    if (map.isEmpty) EmptyMapValueString
    else {
      val values = map.map { case (k, v) => formatter(k, v) }.mkString(", ")
      s"{$values}"
    }
  }
}
