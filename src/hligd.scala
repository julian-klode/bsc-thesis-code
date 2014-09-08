/*
 * hligd.scala - LIGD with HLists
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
import scala.language.reflectiveCalls
import HLists._

/**
 * Implementation of LIGD for Scala using HLists
 *
 * Differences:
 *      - Use HNil to represent Unit
 *      - Use HLists to represent pairs
 */
object HLIGD {

  def geq[A: Rep](a: A, b: A): Boolean = (rep[A], a, b) match {
    case (RBoolean, a, b)                     ⇒ a == b
    case (RInt, a, b)                         ⇒ a == b
    case (RFloat, a, b)                       ⇒ a == b
    case (RChar, a, b)                        ⇒ a == b
    case (RString, a, b)                      ⇒ a == b
    case (RSum(ra, rb), Left(a1), Left(a2))   ⇒ geq(a1, a2)(ra)
    case (RSum(ra, rb), Right(b1), Right(b2)) ⇒ geq(b1, b2)(rb)
    case (RSum(_, _), _, _)                   ⇒ false
    case (RHNil, _, _)                        ⇒ true
    case (RSeq(rx), xs, ys)                   ⇒ xs.length == ys.length
        && !xs.zip(ys).exists((ab) ⇒ !geq(ab._1, ab._2)(rx))
    case (RProd(ra, rb), HCons(a1, b1), HCons(a2, b2)) ⇒
      geq(a1, a2)(ra) && geq(b1, b2)(rb)
    case (r: RType[_, A], t1, t2) ⇒ geq(r.b.from(t1), r.b.from(t2))(r.a)
    case _                        ⇒ false
  }

  /** Representation of types as GADTS. */
  sealed abstract class Rep[T]

  implicit case object RBoolean extends Rep[Boolean]
  implicit case object RHNil extends Rep[HNil]
  implicit case object RInt extends Rep[Int]
  implicit case object RFloat extends Rep[Float]
  implicit case object RChar extends Rep[Char]
  implicit case object RString extends Rep[String]

  /** Represent sums */
  case class RSum[A, B](val a: Rep[A], val b: Rep[B]) extends Rep[Either[A, B]]
  case class RProd[A, B <: HList](val a: Rep[A], val b: Rep[B]) extends Rep[(A :: B)]
  case class RSeq[A](val a: Rep[A]) extends Rep[Seq[A]]

  implicit def rSum[A: Rep, B: Rep]: Rep[Either[A, B]] = RSum(rep[A], rep[B])
  implicit def rProd[A: Rep, B <: HList: Rep]: Rep[(A :: B)] = RProd(rep[A], rep[B])

  /**
   * Represent any type
   *
   * We'd like to use a case class here, but need to be lazy for the rep, as
   * we cannot represent lists and other recursive data structures otherwise.
   * So define a normal class and a custom extractor.
   */
  class RType[C, B](c: ⇒ Rep[C], ep: EP[B, C]) extends Rep[B] {
    lazy val a = c
    lazy val b = ep
  }

  object RType {
    def apply[C, B](a: ⇒ Rep[C], b: EP[B, C]): RType[C, B] = new RType(a, b)
    def unapply[C, B](sum: RType[C, B]) = Some(sum.a, sum.b)
  }

  /** A small factory to make conversions easier */
  def rType[C: Rep, B](from: B ⇒ C, to: C ⇒ B): RType[C, B] = RType(rep[C], EP(from, to))

  /** Isomorphism for converting between types */
  sealed case class EP[B, C](val from: B ⇒ C, val to: C ⇒ B)

  /* ====================================================================
   *         EXAMPLE: LISTS
   * ====================================================================
   */
  case class RList[A](ra: Rep[A]) extends RType(
    RSeq[A](ra),
    EP((list: List[A]) ⇒ list: Seq[A], (seq: Seq[A]) ⇒ seq.toList)
  ) {
    override def equals(other: Any): Boolean = other match {
      case RList(rb) ⇒ this.ra == rb
      case _         ⇒ false
    }
  }
  implicit def rList[A: Rep]: Rep[List[A]] = RList(rep[A])
  def rep[T: Rep] = implicitly[Rep[T]]
}
