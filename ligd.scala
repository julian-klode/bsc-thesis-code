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

/** Implementation of LIGD for Scala as used in 2008.
  *
  * Based on:
  * Comparing Libraries for Generic Programming in Haskell
  * Technical Report UU-CS-2008-010
  * by:
  * Alexey Rodriguez, Johan Jeuring, Patrik Jansson, Alex Gerdes,
  * Oleg Kiselyov, Bruno C. d. S. Oliveira.
  *
  * *Not* based on the code shipped in http://code.haskell.org/generics/LIGD/
  */
object LIGD {

  def geq[A](a: A, b: A)(implicit rep: Rep[A]): Boolean = (rep, a, b) match {
    case (RUnit, (), ())                      ⇒ true
    case (RInt, a, b)                         ⇒ a == b
    case (RFloat, a, b)                       ⇒ a == b
    case (RChar, a, b)                        ⇒ a == b
    case (RString, a, b)                      ⇒ a == b
    case (RSum(ra, rb), Left(a1), Left(a2))   ⇒ geq(a1, a2)(ra)
    case (RSum(ra, rb), Right(b1), Right(b2)) ⇒ geq(b1, b2)(rb)
    case (RSum(_, _), _, _)                   ⇒ false
    case (RProd(ra, rb), (a1, b1), (a2, b2)) ⇒
      geq(a1, a2)(ra) && geq(b1, b2)(rb)
    case (r: RType[_, A], t1, t2) ⇒ geq(r.b.from(t1), r.b.from(t2))(r.a)
    case _                        ⇒ false

  }

  /* Representation of types as GADTS.
   *
   * We'd like to use case classes here, but need to be lazy, as we cannot
   * represent lists and other recursive data structures otherwise. So define
   * normal classes and custom extractors.
   */
  sealed abstract class Rep[+T]

  implicit case object RUnit extends Rep[Unit]
  implicit case object RInt extends Rep[Int]
  implicit case object RFloat extends Rep[Float]
  implicit case object RChar extends Rep[Char]
  implicit case object RString extends Rep[String]

  /** Represent sums */
  case class RSum[A, B](val a: Rep[A], val b: Rep[B]) extends Rep[Either[A, B]]
  case class RProd[A, B](val a: Rep[A], val b: Rep[B]) extends Rep[(A, B)]

  implicit def rSum[A, B](implicit a: Rep[A], b: Rep[B]): Rep[Either[A, B]] = RSum(a, b)
  implicit def rProd[A, B](implicit a: Rep[A], b: Rep[B]): Rep[(A, B)] = RProd(a, b)

  /** Represent any type */
  class RType[C, B](c: ⇒ Rep[C], ep: EP[B, C]) extends Rep[B] {
    lazy val a = c
    lazy val b = ep
  }
  object RType {
    def apply[C, B](a: ⇒ Rep[C], b: EP[B, C]): RType[C, B] = new RType(a, b)
    def unapply[C, B](sum: RType[C, B]) = Some(sum.a, sum.b)
  }

  /** A small factory to make conversions easier */
  def rType[C, B](from: B ⇒ C, to: C ⇒ B)(implicit rc: Rep[C]): RType[C, B] = RType(rc, EP(from, to))

  /** Isomorphism for converting between types */
  sealed case class EP[B, C](val from: B ⇒ C, val to: C ⇒ B)

  /* ====================================================================
   *          EXAMPLE: LISTS
   * ====================================================================
   */
  def fromList[A](list: List[A]): Either[Unit, (A, List[A])] = list match {
    case Nil       ⇒ Left(())
    case (a :: as) ⇒ Right((a, as))
  }

  def toList[A](list: Either[Unit, (A, List[A])]): List[A] = list match {
    case Left(())       ⇒ List.empty
    case Right((a, as)) ⇒ a :: as
  }

  implicit def rList[A](implicit ra: Rep[A]): Rep[List[A]] = RType(
    RSum(RUnit, RProd(ra, rList(ra))),
    EP(fromList[A], toList[A])
  )
}

/** Test cases.
  */
class LIGDTests extends FlatSpec {
  import LIGD._
  val unit = ()
  "geq" should "work" in {
    assert(geq(unit, unit))
  }

  "geq" should "support empty lists" in {
    assert(geq(List.empty: List[Unit], List.empty))
  }

  "geq" should "support non-empty lists" in {
    assert(geq(List(1, 2, 3), List(1, 2, 3)))
    assert(!geq(List(1, 2, 3), List(1, 2, 4)))
    assert(geq(List(unit, unit), List(unit, unit)))
    assert(!geq(List(unit), List(unit, unit)))
  }

  "geq" should "support numbers" in {
    assert(geq(42, 42))
    assert(!geq(42, 7))
  }
  "geq" should "support chars" in {
    assert(geq('4', '4'))
    assert(!geq('4', '2'))
  }
  "geq" should "support strings" in {
    assert(geq("42", "42"))
    assert(!geq("42", "7"))
  }
  "geq" should "support heterogeneous pairs" in {
    assert(geq(("42", 7), ("42", 7)))
    assert(!geq(("42", 7), ("7", 7)))
    assert(!geq(("42", 7), ("42", 42)))
  }
  "geq" should "support homogeneous pairs" in {
    assert(geq(("42", "7"), ("42", "7")))
    assert(!geq(("42", "7"), ("7", "7")))
    assert(!geq(("42", "7"), ("42", "42")))
  }
}
