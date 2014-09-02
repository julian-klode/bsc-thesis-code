/*
 * lidg-company.scala - LIGD types for the company example
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

object LIGDCompany {
  import LIGD._
  import CompanyData._

  /* Let's play a trick */
  def fromFloat(f: Float) = Salary(f)
  def toFloat(s: Salary) = s.salary
  implicit case object RSalary extends RType[Float, Salary](RFloat, EP(toFloat, fromFloat))

  /* The simple one or two element types */
  implicit val rPerson = rType(Person.unapply(_: Person).get, (Person.apply _).tupled)
  implicit val rEmployee = rType(Employee.unapply(_: Employee).get, (Employee.apply _).tupled)

  /* Mutually recursive types: Require annotation and breaking the Rep cycle
   * created by the non-lazy rType. */
  implicit val rDUnit: RType[Either[Dept, Employee], DUnit] = RType(RSum(rDept, rEmployee),
    EP(_ match {
      case PU(per)  ⇒ Right(per)
      case DU(dept) ⇒ Left(dept)
    }, _.fold(DU, PU)))

  implicit val rDept: RType[(String, (Manager, List[DUnit])), Dept] = rType(
    d ⇒ (d.name, (d.manager, d.units)),
    e ⇒ Dept(e._1, e._2._1, e._2._2)
  )

  /* Scala forces this below rDept, otherwise rDept is undefined here */
  implicit val rCompany = rType(Company.unapply(_: Company).get, Company.apply)

  /** Example: Summing all the salaries in a data structure */
  def sumSalaryOld[A: Rep](a: A): Float = (rep[A], a) match {
    case (RSum(ra, rb), Left(a))   ⇒ sumSalaryOld(a)(ra)
    case (RSum(ra, rb), Right(b))  ⇒ sumSalaryOld(b)(rb)
    case (RProd(ra, rb), (a, b))   ⇒ sumSalaryOld(a)(ra) + sumSalaryOld(b)(rb)
    /* Scala does not recognize that salary is a Salary here */
    case (RSalary, salary: Salary) ⇒ RSalary.b.from(salary)
    case (r: RType[_, A], t1)      ⇒ sumSalaryOld(r.b.from(t1))(r.a)
    /* Catch all other cases here */
    case _                         ⇒ 0
  }

  /** Example: Increasing all the salaries in a data structure */
  def incSalary[A: Rep](a: A, by: Float): A = (rep[A], a) match {
    case (RSum(ra, rb), Left(a))   ⇒ Left(incSalary(a, by)(ra))
    case (RSum(ra, rb), Right(b))  ⇒ Right(incSalary(b, by)(rb))
    case (RProd(ra, rb), (a, b))   ⇒ (incSalary(a, by)(ra), incSalary(b, by)(rb))
    case (RSalary, salary: Salary) ⇒ Salary(salary.salary * (1 + by / 100))
    case (r: RType[_, A], t1)      ⇒ r.b.to(incSalary(r.b.from(t1), by)(r.a))
    case (rep, value)              ⇒ value
  }

  def sumSalary[C: Rep](c: C): Float = gfoldl((a: Float, n: Salary) ⇒ (a + n.salary))(0)(c)
  def sumDept[C: Rep](c: C): String = gfoldl((a: String, n: Dept) ⇒ (a + " " + n.name))("")(c)
}
