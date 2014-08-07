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

import scala.language.higherKinds

import EMGM._
import CompanyData._

/**
 * EMGM definitions for company stuff.
 *
 * This is quite verbose. It also seems wrong to use GenericList here, when
 * basically any Generic would do - I don't care about list. But I cannot
 * implement a second GRep[G, List[A]] that only depends on Generic and not
 * on GenericList, as things get ambiguous otherwise.
 */
object EMGMCompany {

  /**
   * Let's define all types of the module here, this makes it easier to
   * read and allows more ad-hoc cases.
   */
  trait GenericCompany[G[_]] extends GenericList[G] {
    def dept: G[Dept] = view(iso)(prod(string)(prod(employee)(list(dunit))))
    def person = view(Iso(Person.unapply(_: Person).get, (Person.apply _).tupled))(prod(string)(string))
    def employee = view(Iso(Employee.unapply(_: Employee).get, (Employee.apply _).tupled))(prod(person)(salary))
    def company = view(Iso(Company.unapply(_: Company).get, Company.apply))(list(dept))
    def dunit = view(iso1)(plus(dept)(employee))
    def salary: G[Salary] = view(Iso(
      (s: Salary) ⇒ s.salary,
      (s: Float) ⇒ Salary(s)
    ))(constr('Salary)(1)(float))
  }

  def iso1: Iso[DUnit, Either[Dept, Employee]] = Iso((_: DUnit) match {
    case PU(per)  ⇒ Right(per)
    case DU(dept) ⇒ Left(dept)
  }, (_: Either[Dept, Employee]).fold(DU, PU))

  def iso: Iso[Dept, (String, (Manager, List[DUnit]))] = Iso(
    (d: Dept) ⇒ (d.name, (d.manager, d.units)),
    (e: (String, (Manager, List[DUnit]))) ⇒ Dept(e._1, e._2._1, e._2._2)
  )

  /* Boilerplate */
  implicit def GRPerson[G[_]](implicit g: GenericCompany[G]) = new GRep[G, Person] {
    override def grep = g.person
  }
  implicit def GREmployee[G[_]](implicit g: GenericCompany[G]) = new GRep[G, Employee] {
    override def grep = g.employee
  }
  implicit def GRCompany[G[_]](implicit g: GenericCompany[G]) = new GRep[G, Company] {
    override def grep = g.company
  }
  implicit def GRDUnit[G[_]](implicit g: GenericCompany[G]) = new GRep[G, DUnit] {
    override def grep = g.dunit
  }
  implicit def GRDept[G[_]](implicit g: GenericCompany[G]) = new GRep[G, Dept] {
    override def grep = g.dept
  }

  /* The incSalary function */
  implicit object MyGTransformSalary extends MyGTransform with GenericCompany[GTransform] with MyGTransformList {
    override def salary = GTransform(x ⇒ Salary(x.salary * 110 / 100))
  }
  /* TODO: How can get a percentage parameter here, like in EMGMCompany ? */
  def incSalary[T](a: T)(implicit r: GRep[GTransform, T]): T =
    r.grep.transform(a)

  /* WTF ? Even more boilerplate. I want geqList to just work, it needs to
   * know nothing about company stuff. */
  implicit object GEqCompany extends MyGEq with GenericCompany[GEq]
}
