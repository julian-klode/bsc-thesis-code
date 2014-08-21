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

import scala.language.implicitConversions
import org.scalatest._

class LIGDCompanyTests extends FlatSpec {
  import LIGD._
  import CompanyData._
  import LIGDCompany._

  "sum" should "be 111000.0" in {
    assert(geq(sumSalaryOld(genCom), 111000.0F))
    assert(geq(sumSalary(genCom), 111000.0F))
  }
  "salary" should "be increased by 10%" in {
    assert(geq(incSalary(genCom, 10), expCom))
    assert(geq(everywhere((i: Salary) ⇒ Salary(i.salary * 1.1F))(genCom), expCom))
  }

  "gfoldl2" should "work" in {
    assert(gMinInt((List(1, 2, 3), ((List(3, 4, 5), 9), 7))) == Some(1))
  }
}

class EMGMCompanyTests extends FlatSpec {
  import EMGM._
  import CompanyData._
  import EMGMCompany._

  "sum" should "be 111000.0" in {
    assert(geq(sumSalary(genCom), 111000.0F))
  }

  "salary" should "be increased by 10%" in {
    assert(incSalary(genCom) == expCom)
    assert(geqList(genCom, genCom))
  }

}
class ShapelessCompanyTests extends FlatSpec {
  import shapeless._
  import poly._
  import CompanyData._

  object combine extends Poly {
    implicit def caseSalaryFloat = use((s: Salary, f: Float) ⇒ s.salary + f)
  }

  "salary" should "be increased by 10%" ignore {
    // We need to create a poly object here, shapeless fails horribly otherwise
    object incSalary extends ->((i: Salary) ⇒ Salary(i.salary * 1.1F))
    assert(everywhere(incSalary)(genCom) == expCom)
  }

}
