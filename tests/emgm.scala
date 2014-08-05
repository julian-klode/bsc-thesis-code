/*
 * masses.scala - Implementation of Generics for the Masses in Scala
 *
 * Copyright 2014 Julian Andres Klode <klode@mathematik.uni-marburg.de>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import org.scalatest._

class EMGMTests extends FlatSpec {
  import EMGM._
  val unit = ()
  "geq" should "work" in {
    assert(geq(unit, unit))
  }

  it should "support empty lists" in {
    assert(geq(List.empty: List[Unit], List.empty))
  }

  it should "support non-empty lists" in {
    assert(geq(List(1, 2, 3), List(1, 2, 3)))
    assert(!geq(List(1, 2, 3), List(1, 2, 4)))
    assert(geq(List(unit, unit), List(unit, unit)))
    assert(!geq(List(unit), List(unit, unit)))
  }

  it should "support numbers" in {
    assert(geq(42, 42))
    assert(!geq(42, 7))
  }
  it should "support chars" in {
    assert(geq('4', '4'))
    assert(!geq('4', '2'))
  }
  it should "support strings" in {
    assert(geq("42", "42"))
    assert(!geq("42", "7"))
  }
  it should "support heterogeneous pairs" in {
    assert(geq(("42", 7), ("42", 7)))
    assert(!geq(("42", 7), ("7", 7)))
    assert(!geq(("42", 7), ("42", 42)))
  }
  it should "support homogeneous pairs" in {
    assert(geq(("42", "7"), ("42", "7")))
    assert(!geq(("42", "7"), ("7", "7")))
    assert(!geq(("42", "7"), ("42", "42")))
  }
}
