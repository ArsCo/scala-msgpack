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

/** Message pack value stringifier.
  *
  * @author Arsen Ibragimov (ars)
  * @since 0.0.1
  */
trait ValueStringifier {

  val UnknownValueString = "???"

  /** Converts message pack value to human-readable string.
    *
    * @param value the value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatValue(value: Value): String = {
    requireNotNull(value, "value")

    val t = value.getValueType
    if (t.isBooleanType) {
      formatBoolean(value.asBooleanValue())

    } else if (t.isIntegerType) {
      formatInteger(value.asIntegerValue())

    } else if (t.isStringType) {
      formatString(value.asStringValue())

    } else if (t.isArrayType) {
      formatArray(value.asArrayValue())

    } else if (t.isBinaryType) {
      formatBinary(value.asBinaryValue())

    } else if (t.isMapType) {
      formatMap(value.asMapValue())

    } else if (t.isExtensionType) {
      formatExtension(value.asExtensionValue())

    } else if (t.isFloatType) {
      formatFloat(value.asFloatValue())

    } else if (t.isNilType) {
      formatNilValue(value.asNilValue())

    } else if (t.isNumberType) {
      formatNumber(value.asNumberValue())

    } else if (t.isRawType) {
      formatRaw(value.asRawValue())

    } else {
      UnknownValueString
    }
  }

  /**
    * Converts message pack boolean value to human-readable string.
    *
    * @param value the message pack boolean value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatBoolean(value: BooleanValue): String

  /**
    * Converts message pack integer value to human-readable string.
    *
    * @param value the message pack integer value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatInteger(value: IntegerValue): String

  /**
    * Converts message pack string value to human-readable string.
    *
    * @param value the message pack string value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatString(value: StringValue): String

  /**
    * Converts message pack array value to human-readable string.
    *
    * @param value the message pack array value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatArray(value: ArrayValue): String

  /**
    * Converts message pack binary value to human-readable string.
    *
    * @param value the message pack binary value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatBinary(value: BinaryValue): String

  /**
    * Converts message pack map value to human-readable string.
    *
    * @param value the message pack map value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatMap(value: MapValue): String

  /**
    * Converts message pack extension value to human-readable string.
    *
    * @param value the message pack extension value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatExtension(value: ExtensionValue): String

  /**
    * Converts message pack float value to human-readable string.
    *
    * @param value the message pack float value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatFloat(value: FloatValue): String

  /**
    * Converts message pack nil-value to human-readable string.
    *
    * @param value the message pack nil-value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatNilValue(value: NilValue): String

  /**
    * Converts message pack number-value to human-readable string.
    *
    * @param value the message pack number-value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatNumber(value: NumberValue): String

  /**
    * Converts message pack raw value to human-readable string.
    *
    * @param value the message pack raw value (must be non-null)
    *
    * @return the resulting human-readable string (non-null)
    */
  def formatRaw(value: RawValue): String
}
