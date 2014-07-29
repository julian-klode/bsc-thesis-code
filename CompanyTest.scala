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

import LIGD._

object LIGDCompany {
  import CompanyData._

  /* Let's play a trick */
  def fromFloat(f: Float) = Salary(f)
  def toFloat(s: Salary) = s.salary
  implicit case object RSalary extends RType[Float, Salary](RFloat, EP(toFloat, fromFloat))

  /* The simple one or two element types */
  implicit def rPerson = rType(Person.unapply(_: Person).get, (Person.apply _).tupled)
  implicit def rEmployee = rType(Employee.unapply(_: Employee).get, (Employee.apply _).tupled)
  implicit def rCompany = rType(Company.unapply(_: Company).get, Company.apply)

  /* Mutually recursive types, require annotation, also need to break loop */
  implicit def rDUnit: RType[Either[Dept, Employee], DUnit] = RType(RSum(rDept, rEmployee),
    EP(_ match {
      case PU(per)  ⇒ Right(per)
      case DU(dept) ⇒ Left(dept)
    }, _.fold(DU, PU)))

  implicit def rDept: RType[(String, (Manager, List[DUnit])), Dept] = rType(
    d ⇒ (d.name, (d.manager, d.units)),
    e ⇒ Dept(e._1, e._2._1, e._2._2)
  )

  /** Example: Summing all the salaries in a data structure */
  def sumSalary[A](a: A)(implicit rep: Rep[A]): Float = (rep, a) match {
    case (RSum(ra, rb), Left(a))   ⇒ sumSalary(a)(ra)
    case (RSum(ra, rb), Right(b))  ⇒ sumSalary(b)(rb)
    case (RProd(ra, rb), (a, b))   ⇒ sumSalary(a)(ra) + sumSalary(b)(rb)
    /* Scala does not recognize that salary is a Salary here */
    case (RSalary, salary: Salary) ⇒ RSalary.b.from(salary)
    case (r: RType[_, A], t1)      ⇒ sumSalary(r.b.from(t1))(r.a)
    /* Catch all other cases here */
    case _                         ⇒ 0
  }

  /** Example: Increasing all the salaries in a data structure */
  def incSalary[A](a: A, by: Float)(implicit rep: Rep[A]): A = (rep, a) match {
    case (RSum(ra, rb), Left(a))   ⇒ Left(incSalary(a, by)(ra))
    case (RSum(ra, rb), Right(b))  ⇒ Right(incSalary(b, by)(rb))
    case (RProd(ra, rb), (a, b))   ⇒ (incSalary(a, by)(ra), incSalary(b, by)(rb))
    case (RSalary, salary: Salary) ⇒ Salary(salary.salary * (1 + by / 100))
    case (r: RType[_, A], t1)      ⇒ r.b.to(incSalary(r.b.from(t1), by)(r.a))
    case (rep, value)              ⇒ value
  }

}

class LIGDCompanyTests extends FlatSpec {
  import CompanyData._
  import LIGDCompany._

  "sum" should "be 111000.0" in {
    assert(geq(sumSalary(genCom), 111000.0F))
  }
  "salary" should "be increased by 10%" in {
    assert(geq(incSalary(genCom, 10), expCom))
  }
}