/*
 * test.scala - Main program / benchmark tests
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

class TestTest extends FlatSpec {
  import Main._

  "benchmark implementations" should "produce expected results" in {
    for (lib ← L.values) {
      if (lib != L.None) {
        for ((testname, test) ← tests) {
          test(lib) match {
            case Some(v) ⇒ assert(v == test(L.None).get,
              "in test $testname in library ${lib.toString}")
            case None ⇒ true
          }
        }
      }
    }
  }
}
