/*
 * data.scala - Implementation of LIGD for Scala.
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

/**
 * Example: The company data
 *
 * Operations to try:
 * Increase all salaries by 10%
 * Sum up all salaries
 */
object CompanyData {

  case class Company(val depts: List[Dept])
  case class Dept(val name: Name, val manager: Manager, val units: List[DUnit])
  sealed trait DUnit
  case class PU(val person: Employee) extends DUnit
  case class DU(val dept: Dept) extends DUnit
  case class Employee(val person: Person, val salary: Salary)
  case class Person(val name: Name, val address: Address)
  case class Salary(val salary: Float)

  type Manager = Employee
  type Name = String
  type Address = String

  val ralf = Employee(Person("Ralf", "Amsterdam"), Salary(8000))
  val joost = Employee(Person("Joost", "Amsterdam"), Salary(1000))
  val marlow = Employee(Person("Marlow", "Cambridge"), Salary(2000))
  val blair = Employee(Person("Blair", "London"), Salary(100000))

  /** Start values */
  val genCom: Company =
    Company(List(
      Dept("Research", ralf, List(PU(joost), PU(marlow))),
      Dept("Strategy", blair, Nil)))

  /** Expected values after 10% increase */
  val ralf2 = Employee(Person("Ralf", "Amsterdam"), Salary(8800))
  val joost2 = Employee(Person("Joost", "Amsterdam"), Salary(1100))
  val marlow2 = Employee(Person("Marlow", "Cambridge"), Salary(2200))
  val blair2 = Employee(Person("Blair", "London"), Salary(110000))
  val expCom: Company =
    Company(List(
      Dept("Research", ralf2, List(PU(joost2), PU(marlow2))),
      Dept("Strategy", blair2, Nil)))

}
