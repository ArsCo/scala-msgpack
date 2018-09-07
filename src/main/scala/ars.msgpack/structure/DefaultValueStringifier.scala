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
import org.msgpack.value._

import scala.collection.JavaConversions._


/** Default message pack value stringifier.
  *
  * @author Arsen Ibragimov (ars)
  * @since 0.0.1
  */
class DefaultValueStringifier extends ValueStringifier with ScalaTypeStringifier {

  val tupleFormatter: Formatter[(Value, Value)] = formatTuple(formatValue, formatValue)

  override def formatBoolean(value: BooleanValue): String = {
    requireNotNull(value, "value")
    formatBoolean(value.getBoolean)
  }

  override def formatInteger(value: IntegerValue): String = {
    requireNotNull(value, "value")
    formatBigInt(value.asBigInteger())
  }

  override def formatString(value: StringValue): String = {
    requireNotNull(value, "value")
    value.asString
  }

  override def formatArray(value: ArrayValue): String = {
    requireNotNull(value, "value")
    formatList[Value](formatValue)(value.list().toList)
  }

  override def formatBinary(value: BinaryValue): String = {
    requireNotNull(value, "value")
    formatByteArray(value.asByteArray())
  }

  override def formatMap(value: MapValue): String = {
    requireNotNull(value, "value")

    formatMap(tupleFormatter)(value.map().toMap)
  }

  override def formatExtension(value: ExtensionValue): String = {
    requireNotNull(value, "value")
    if (value.getType != -1) {
      value.getData // TODO
    }
    UnknownValueString // TODO
  }

  override def formatFloat(value: FloatValue): String = {
    requireNotNull(value, "value")
    UnknownValueString
  }

  override def formatNilValue(value: NilValue): String = {
    requireNotNull(value, "value")
    UnknownValueString
  }

  override def formatNumber(value: NumberValue): String = {
    requireNotNull(value, "value")
    UnknownValueString
  }

  override def formatRaw(value: RawValue): String = {
    requireNotNull(value, "value")
    UnknownValueString
  }
}
