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

import org.scalameter._

object Main {
  import CompanyData._
  import LIGDCompany._
  import HLIGDCompany._
  import EMGMCompany._
  import shapeless.poly._

  object incS extends ->((i: Salary) ⇒ Salary(i.salary * 1.1F))

  val manager = Employee(Person("Manager", "Manager Address"), Salary(420))
  /* Generates count + count**2 salaries */
  def mkComp(count: Int) = {
    def mkUnits(unitCount: Int) = {
      for (i ← 43 to unitCount + 42)
        yield PU(Employee(Person("name", "address"), Salary(i * 10)))
    }

    def mkDept(depCount: Int) = {
      for (i ← 43 to depCount + 42)
        yield Dept("dept", manager, units = mkUnits(depCount).toList)
    }
    Company(mkDept(count).toList)
  }

  /* Will have 8 + 8 * 8 = 72 salaries */
  val com1 = mkComp(256)
  val incEverywhere = shapeless.everywhere(incS)

  def company(lib: L.Value) = lib match {
    case L.Shapeless ⇒ Some(incEverywhere(com1))
    case L.LIGD      ⇒ Some(LIGDCompany.incSalary(com1, 10)(LIGDCompany.rCompany))
    case L.HLIGD     ⇒ Some(HLIGDCompany.incSalary(com1, 10)(HLIGDCompany.rCompany))
    case L.EMGM      ⇒ Some(EMGMCompany.incSalary(com1))
    case L.Direct ⇒ {
      def incSalary(c: Company) = Company(c.depts.map(incSalaryD))
      def incSalaryD(d: Dept) = Dept(d.name, incSalaryE(d.manager), d.units.map(incSalaryU))
      def incSalaryU(du: DUnit): DUnit = du match {
        case DU(d) ⇒ DU(incSalaryD(d))
        case PU(p) ⇒ PU(incSalaryE(p))
      }
      def incSalaryE(e: Employee) = Employee(e.person, incSalaryS(e.salary))
      def incSalaryS(s: Salary) = Salary(s.salary * 110 / 100)

      Some(incSalary(com1))
    }
    case L.None ⇒ {
      def incSalary(c: Company) = Company(c.depts.map(incSalaryD))
      def incSalaryD(d: Dept) = Dept(d.name, incSalaryE(d.manager), d.units.map(incSalaryU))
      def incSalaryU(du: DUnit): DUnit = du match {
        case DU(d) ⇒ DU(incSalaryD(d))
        case PU(p) ⇒ PU(incSalaryE(p))
      }
      def incSalaryE(e: Employee) = Employee(e.person, incSalaryS(e.salary))
      def incSalaryS(s: Salary) = Salary(s.salary * 110 / 100)

      Some(incSalary(com1))
    }
    case _ ⇒ None
  }

  val list1 = (1 until 256 * 256).toList
  val list2 = (1 until 256 * 256 + 1).toList

  def geq(lib: L.Value) = lib match {
    case L.None   ⇒ Some(false)
    case L.Direct ⇒ Some(list1.equals(list2))
    case L.LIGD   ⇒ Some(LIGD.geq(list1, list2))
    case L.HLIGD  ⇒ Some(HLIGD.geq(list1, list2))
    case L.EMGM   ⇒ Some(EMGM.geq(list1, list2))
    case _        ⇒ None
  }

  def min(lib: L.Value) = lib match {
    case L.None      ⇒ Some(1)
    case L.LIGD      ⇒ Some(LIGD.min(list2))
    case L.EMGM      ⇒ Some(EMGM.min(list2))
    case L.Direct    ⇒ Some(list2.foldLeft(Int.MaxValue)(scala.math.min))
    case L.Shapeless ⇒ Some(ShapelessFun.min(list2))
    case _           ⇒ None
  }
  def sum(lib: L.Value) = lib match {
    case L.None      ⇒ Some(list2.foldLeft(0)(_ + _))
    case L.Direct    ⇒ Some(list2.foldLeft(0)(_ + _))
    case L.LIGD      ⇒ Some(LIGD.foldl(list2)(0)(_ + _))
    case L.EMGM      ⇒ Some(EMGM.sum(list2))
    case L.Shapeless ⇒ Some(ShapelessFun.sum(list2))
    case _           ⇒ None
  }

  def bench[A](lib: String, test: String, code: ⇒ Option[A]) = code match {
    case Some(_) ⇒ printf(" & %15s", time(code))
    case None    ⇒ printf(" & %15s", "N/A")
  }

  /* Benchmark configuration */
  val scalameterConfig = config(
    Key.exec.benchRuns -> 30,
    Key.verbose -> false
  ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC with Measurer.OutlierElimination
    }

  def time[A](a: ⇒ A) = {
    /* This reduces the GC runs during scalameter benchmarking slightly */
    System.gc()
    System.gc()
    val result = scalameterConfig measure { a }
    if (result >= 1)
      ("%.1f ms".format(result))
    else
      ("%.1f μs".format(result * 1000.0))
  }

  object L extends Enumeration {
    type L = Value
    val Direct, None, Shapeless, LIGD, HLIGD, EMGM = Value
  }

  val tests = List(
    ("company", company _),
    ("geq", geq _),
    ("min", min _),
    ("sum", sum _)
  )

  def main(args: Array[String]) {
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
      if (lib != L.None) {
        printf("%-10s", lib.toString())
        for ((testname, test) ← tests) {
          test(lib) match {
            case Some(result) ⇒ assert(result == test(L.None).get, s"${lib.toString}: $testname failed. Expected ${test(L.None)}, received ${test(lib)}")
            case None         ⇒ true
          }

          bench(lib.toString(), testname, test(lib))
        }
        println("\\\\")
      }
    }
    println("\\end{tabular}")
  }

}
