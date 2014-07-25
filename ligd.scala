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

  def geq[A](rep: Rep[A], a: A, b: A): Boolean = (rep, a, b) match {
    case (RUnit, (), ())                      ⇒ true
    case (RSum(ra, rb), Left(a1), Left(a2))   ⇒ geq(ra, a1, a2)
    case (RSum(ra, rb), Right(b1), Right(b2)) ⇒ geq(rb, b1, b2)
    case (RSum(_, _), _, _)                   ⇒ false
    case (RProd(ra, rb), (a1, b1), (a2, b2)) ⇒
      geq(ra, a1, a2) && geq(rb, b1, b2)
    case (r: RType[_, A], t1, t2) ⇒ geq(r.a, r.b.from(t1), r.b.from(t2))
    case _                        ⇒ false

  }

  /* Representation of types as GADTS.
   *
   * We'd like to use case classes here, but need to be lazy, as we cannot
   * represent lists and other recursive data structures otherwise. So define
   * normal classes and custom extractors.
   */
  sealed abstract class Rep[+T]

  case object RUnit extends Rep[Unit]

  /** Represent sums */
  case class RSum[A, B](val a: Rep[A], val b: Rep[B]) extends Rep[Either[A, B]]
  case class RProd[A, B](val a: Rep[A], val b: Rep[B]) extends Rep[(A, B)]

  /** Represent any type */
  sealed class RType[C, B](c: ⇒ Rep[C], ep: EP[B, C]) extends Rep[B] {
    lazy val a = c
    lazy val b = ep
  }
  object RType {
    def apply[C, B](a: ⇒ Rep[C], b: EP[B, C]): RType[C, B] = new RType(a, b)
    def unapply[C, B](sum: RType[C, B]) = Some(sum.a, sum.b)
  }

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

  def rList[A](ra: ⇒ Rep[A]): Rep[List[A]] = RType(
    RSum(RUnit, RProd(ra, rList(ra))),
    EP(fromList[A], toList[A])
  )
}

/** Test cases.
  */
class LIGDTests extends FlatSpec {
  import LIGD._
  "geq" should "work" in {
    assert(geq(RUnit, (), ()))
  }

  "geq" should "support empty lists" in {
    assert(geq(rList(RUnit), Nil, Nil))
  }
  "geq" should "support non-empty lists" in {
    assert(geq(rList(rList(RUnit)), List(Nil, Nil), List(Nil, Nil)))
    assert(!geq(rList(rList(RUnit)), List(Nil), List(Nil, Nil)))
    assert(!geq(rList(rList(RUnit)), List(Nil), Nil))
  }
}
