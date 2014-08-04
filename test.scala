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

object Main {
  import CompanyData._
  import LIGDCompany._

  def incS(i: Salary) = Salary(i.salary * 1.1F)

  def company(lib: L.Value) = lib match {
    case L.Shapeless ⇒ Some(shapeless.everywhere(incS _)(CompanyData.genCom))
    case L.LIGD      ⇒ Some(LIGDCompany.incSalary(CompanyData.genCom, 10))
    case L.Direct ⇒ {
      def incSalary(c: Company) = Company(c.depts.map(incSalaryD))
      def incSalaryD(d: Dept) = Dept(d.name, incSalaryE(d.manager), d.units.map(incSalaryU))
      def incSalaryU(du: DUnit): DUnit = du match {
        case DU(d) ⇒ DU(incSalaryD(d))
        case PU(p) ⇒ PU(incSalaryE(p))
      }
      def incSalaryE(e: Employee) = Employee(e.person, incSalaryS(e.salary))
      def incSalaryS(s: Salary) = Salary(s.salary * 110 / 100)

      Some(incSalary(genCom))
    }
    case _ ⇒ None
  }

  def geq(lib: L.Value) = lib match {
    case L.Direct ⇒ Some(List(1, 2, 3, 4, 5).equals(List(1, 2, 3, 4, 5, 6)))
    case L.LIGD   ⇒ Some(LIGD.geq(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5, 6)))
    case L.EMGM   ⇒ Some(EMGM.geq(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5, 6)))
    case _        ⇒ None
  }

  def min(lib: L.Value) = lib match {
    case L.LIGD   ⇒ Some(LIGD.foldl(List(4, 6, 3, 1, 2, 9, 8, 7, 6, 5, 10, 11))(0)(scala.math.min))
    case L.Direct ⇒ Some(List(4, 6, 3, 1, 2, 9, 8, 7, 6, 5, 10, 11).foldLeft(0)(scala.math.min))
    case _        ⇒ None
  }
  def sum(lib: L.Value) = lib match {
    case L.Direct ⇒ Some(List(4, 6, 3, 1, 2, 9, 8, 7, 6, 5, 10, 11).foldLeft(0)(_ + _))
    case L.LIGD   ⇒ Some(LIGD.foldl(List(4, 6, 3, 1, 2, 9, 8, 7, 6, 5, 10, 11))(0)(_ + _))
    case _        ⇒ None
  }

  def bench[A](lib: String, test: String, code: ⇒ Option[A]) = code match {
    case Some(_) ⇒ printf(" & %15s", time(code))
    case None    ⇒ printf(" & %15s", "N/A")
  }

  def time[A](a: ⇒ A) = {
    val n = 300000
    var times: Long = 0
    /* JIT compile */
    for (_ ← 1 to n) {
      val res = a
    }
    for (_ ← 1 to n) {
      val now = System.nanoTime
      val res = a
      times += (System.nanoTime - now)
    }
    val result = times / n
    ("%d ns".format(result))
  }

  object L extends Enumeration {
    type L = Value
    val Direct, Shapeless, LIGD, EMGM = Value
  }

  def main(args: Array[String]) {
    assert(company(L.Direct).get == expCom)

    val tests = List(("company", company _), ("geq", geq _), ("min", min _),
      ("sum", sum _))
    printf("\\begin{tabular}{c")
    for (lib ← tests) {
      printf("|r")
    }
    printf("}\n")
    printf("%-10s", "\\bf{test}")
    for ((testname, test) ← tests) {
      printf(" & %-15s", "\\bf{" + testname + "}")
    }
    println("\\\\")
    println("\\hline")
    for (lib ← L.values) {
      printf("%-10s", lib.toString())
      for ((testname, test) ← tests) {

        bench(lib.toString(), testname, test(lib))
      }
      println("\\\\")
    }
    println("\\end{tabular}")
  }

}
