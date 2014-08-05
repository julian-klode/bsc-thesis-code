/*
 * lidg.scala - Implementation of LIGD for Scala.
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

/**
 * Test cases.
 */
class LIGDTests extends FlatSpec {
  import LIGD._
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

  "folding" should "have working gfoldl examples" in {
    val div = (_: Int) / (_: Int)
    val max = scala.math.max(_: Int, _: Int)

    assert(gfoldl(div)(64)(List(4, 2, 4)) == 2)
    assert(gfoldl(div)(3)(List.empty: List[Int]) == 3)
    assert(gfoldl(max)(5)(List(1, 2, 3, 4)) == 5)
    assert(gfoldl(max)(5)(List(1, 2, 3, 4, 5, 6, 7)) == 7)
    assert(gfoldl((x: Int, y: Int) ⇒ 2 * x + y)(4)(List(1, 2, 3)) == 43)

    /* foldl is much nicer, due to the C[N] and the re-ordered arguments */
    assert(foldl(List(4, 2, 4))(64)(_ / _) == 2)
    assert(foldl(List.empty: List[Int])(3)(_ / _) == 3)
    assert(foldl(List(1, 2, 3, 4))(5)(scala.math.max) == 5)
    assert(foldl(List(1, 2, 3, 4, 5, 6, 7))(5)(scala.math.max) == 7)
    assert(foldl(List(1, 2, 3))(4)((x, y) ⇒ 2 * x + y) == 43)
  }

  it should "have working gfoldr examples" in {
    val max = scala.math.max(_: Int, _: Int)
    assert(gfoldr((_: Int) + (_: Int))(5)(List(1, 2, 3, 4)) == 15)
    assert(gfoldr((_: Int) / (_: Int))(2)(List(8, 12, 24, 4)) == 8)
    assert(gfoldr((_: Int) / (_: Int))(3)(List.empty: List[Int]) == 3)
    assert(gfoldr((_: Boolean) && (_: Boolean))(true)(List(1 > 2, 3 > 2, 5 == 5)) == false)
    assert(gfoldr(max)(18)(List(3, 6, 12, 4, 55, 11)) == 55)
    assert(gfoldr(max)(111)(List(3, 6, 12, 4, 55, 11)) == 111)
    assert(gfoldr((a: Int, b: Int) ⇒ (a + b) / 2)(54)(List(12, 4, 10, 6)) == 12)
  }
}
