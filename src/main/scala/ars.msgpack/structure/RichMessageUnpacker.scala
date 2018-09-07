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

import org.msgpack.core.MessageUnpacker
import org.msgpack.value.{Value, ValueType}

import scala.language.implicitConversions

/** Additional methods for [[MessageUnpacker]].
  *
  * @author Arsen Ibragimov (ars)
  * @since 0.0.1
  */
class RichMessageUnpacker(val unpacker: MessageUnpacker) extends AnyVal {

  /**
    * Converts [[MessageUnpacker]] to value sequence.
    *
    * @return the value sequencе (non-null)
    */
  def toValueSeq: Seq[Value] = {
    var seq = Seq[Value]()
    while (unpacker.hasNext) {
      seq +:= unpacker.unpackValue()
    }
    seq.reverse
  }
}

object implicits {
  implicit def unpacker2rich(unpacker: MessageUnpacker): RichMessageUnpacker = new RichMessageUnpacker(unpacker)
}
