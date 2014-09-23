/*
 * shapeless.scala - Example queries using Shapeless
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
 * Test functions implemented using shapeless
 */
object ShapelessFun {
  import shapeless._
  import CompanyData._

  object salary extends Poly1 {
    implicit def caseSalary = at[Salary](s ⇒ List(s))
    implicit def default[T] = at[T](_ ⇒ List[Salary]())
  }
  object queryInt extends Poly1 {
    implicit def caseSalary = at[Int](s ⇒ List(s))
    implicit def default[T] = at[T](_ ⇒ List[Int]())
  }

  object int1 extends Poly1 {
    implicit def caseInt = at[Int](s ⇒ s)
    implicit def default[T] = at[T](_ ⇒ 1)
  }
  object int0 extends Poly1 {
    implicit def caseInt = at[Int](s ⇒ s)
    implicit def default[T] = at[T](_ ⇒ 0)
  }

  object plus extends Poly2 {
    implicit val caseInt = at[Int, Int](_ + _)
    implicit val caseLong = at[Long, Long](_ + _)
    implicit val caseFloat = at[Float, Float](_ + _)
    implicit val caseDouble = at[Double, Double](_ + _)
    implicit val caseString = at[String, String](_ + _)
    implicit def caseList[T] = at[List[T], List[T]](_ ::: _)
  }
  object prod extends Poly2 {
    implicit val caseInt = at[Int, Int](_ * _)
    implicit val caseLong = at[Long, Long](_ * _)
    implicit val caseFloat = at[Float, Float](_ * _)
    implicit val caseDouble = at[Double, Double](_ * _)
  }

  object intmax extends Poly1 {
    implicit def caseInt = at[Int](s ⇒ s)
    implicit def default[T] = at[T](_ ⇒ Int.MaxValue)
  }
  object minimum extends Poly2 {
    implicit val caseInt = at[Int, Int](scala.math.min)
    implicit val caseDouble = at[Double, Double](scala.math.min)
  }
  val min = everything(intmax)(minimum)

  val salaries = everything(salary)(plus)
  val ints = everything(queryInt)(plus)
  val product = everything(int1)(prod)
  val sum = everything(int0)(plus)

  /* Example: sumSalary */
  object extractSalary extends Poly1 {
    implicit def caseSalary = at[Salary](s ⇒ s.salary)
    implicit def default[T] = at[T](_ ⇒ 0.0F)
  }
  object addFloat extends Poly2 {
    implicit val caseFloat = at[Float, Float](_ + _)
  }

  val sumSalary = everything(extractSalary)(addFloat)
}
