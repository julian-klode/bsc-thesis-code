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

import scala.language.higherKinds

import org.scalatest._

/**
 * Implementation of LIGD for Scala as used in 2008.
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
    case (RBoolean, a, b)                     ⇒ a == b
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

  /** Representation of types as GADTS. */
  sealed abstract class Rep[T]

  implicit case object RBoolean extends Rep[Boolean]
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

    /* Will stack overflow on infinite types, but better than nothing */
    override def equals(obj: Any): Boolean = obj match {
      case RType(c1, ep1) ⇒ c1 == c
      case _              ⇒ false
    }
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
   *         EXAMPLE: LISTS
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

  case class RList[A](ra: Rep[A]) extends RType(
    RSum(RUnit, RProd(ra, rList(ra))),
    EP(fromList[A], toList[A])
  ) {
    override def equals(other: Any): Boolean = other match {
      case RList(rb) ⇒ this.ra == rb
      case _         ⇒ false
    }
  }

  implicit def rList[A](implicit ra: Rep[A]): Rep[List[A]] = RList(ra)

  /**
   * Usage: foldl(fun: (A, N) => A)(unit: A)(container: C)
   *
   * A simple left fold over some container. The function needs to have both
   * argument types annotated, otherwise Scala won't be able to infer the
   * representation of N.
   *
   * Type notes: A means accumulator, C means container, N means needle
   *
   */
  def foldl[A, C, N](fun: (A, N) ⇒ A)(unit: A)(c: C)(implicit rep: Rep[C], rn: Rep[N]): A = (rep, c) match {
    case (r, v) if r == rn        ⇒ fun(unit, v.asInstanceOf[N])
    case (RSum(ra, rb), Left(x))  ⇒ foldl(fun)(unit)(x)(ra, rn)
    case (RSum(ra, rb), Right(x)) ⇒ foldl(fun)(unit)(x)(rb, rn)
    case (RProd(ra, rb), (x, y))  ⇒ foldl(fun)(foldl(fun)(unit)(x)(ra, rn))(y)(rb, rn)
    case (r: RType[_, C], t1)     ⇒ foldl(fun)(unit)(r.b.from(t1))(r.a, rn)
    case _                        ⇒ unit
  }

  /**
   * Usage: foldr(fun: (A, N) => A)(unit: A)(container: C)
   *
   * A simple right fold over some container. The function needs to have both
   * argument types annotated, otherwise Scala won't be able to infer the
   * representation of N.
   *
   * Type notes: A means accumulator, C means container, N means needle
   *
   */
  def foldr[A, C, N](fun: (N, A) ⇒ A)(unit: A)(c: C)(implicit rep: Rep[C], rn: Rep[N]): A = (rep, c) match {
    case (r, v) if r == rn        ⇒ fun(v.asInstanceOf[N], unit)
    case (RSum(ra, rb), Left(x))  ⇒ foldr(fun)(unit)(x)(ra, rn)
    case (RSum(ra, rb), Right(x)) ⇒ foldr(fun)(unit)(x)(rb, rn)
    case (RProd(ra, rb), (x, y))  ⇒ foldr(fun)(foldr(fun)(unit)(y)(rb, rn))(x)(ra, rn)
    case (r: RType[_, C], t1)     ⇒ foldr(fun)(unit)(r.b.from(t1))(r.a, rn)
    case _                        ⇒ unit
  }

  /**
   * Find all instances of a given type in an object.
   *
   * @param rn The representation of objects were a looking for
   * @param c The containing object we are searching in
   * @param rc (Implicit) Representation of c
   */
  def findAll[N, C](rn: Rep[N])(c: C)(implicit rc: Rep[C]) = foldr(
    (x: N, xs: List[N]) ⇒ x :: xs
  )(List.empty)(c)(rc, rn)

  /**
   * A generic sum that can sum both integers and floats (and products
   *
   * @param rt The representation of T values we want to build sums of
   * @param c  The container containing (or not) or T values
   */
  def gSum[T, C](rt: Rep[T], c: C)(implicit rep: Rep[C]): T = {
    foldl(add(rt))(zero(rt))(c)(rep, rt)
  }

  /**
   * A generic sum that can sum both integers and floats (and products
   *
   * @param rt The representation of T values we want to build sums of
   * @param c  The container containing (or not) or T values
   */
  def sum[T, C[_]](c: C[T])(implicit rep: Rep[C[T]], rt: Rep[T]): T = {
    foldl(add(rt))(zero(rt))(c)(rep, rt)
  }

  /** Helper for sumOf: Add two objects of same type */
  def add[T](rep: Rep[T])(a: T, b: T): T = rep match {
    case RInt          ⇒ a + b
    case RFloat        ⇒ a + b
    case RProd(ra, rb) ⇒ (add(ra)(a._1, b._1), add(rb)(a._2, b._2))
    case _             ⇒ throw new Exception("Unknown types")
  }

  /** Helper for sumOf: Return a zero value */
  def zero[T](implicit rep: Rep[T]): T = rep match {
    case RInt          ⇒ 0
    case RFloat        ⇒ 0F
    case RProd(ra, rb) ⇒ (zero(ra), zero(rb))
    case _             ⇒ throw new Exception("Unknown types")
  }

  /** Helper: If a is None, return n, otherwise the minimum of both */
  def _minOrNone(a: Option[Int], n: Int): Option[Int] = Some(a.fold(n)(scala.math.min(_, n)))
  /** Find the minimum Integer in an object. If none exists, return None */
  def gMinInt[C](c: C)(implicit rep: Rep[C]): Option[Int] = foldl(_minOrNone)(None)(c)
  /** Find the minimum integer in a container of integers */
  def minInt[C[_]](c: C[Int])(implicit rep: Rep[C[Int]]): Option[Int] = gMinInt(c)(rep)
}

/**
 * Test cases.
 */
class LIGDTests extends FlatSpec {
  import LIGD._
  val unit = ()
  "geq" should "work" in {
    assert(geq(unit, unit))
  }

  it should "support empty lists" in {
    assert(geq(List.empty: List[Unit], List.empty))
  }

  it should "support non-empty lists" in {
    assert(geq(List(1, 2, 3), List(1, 2, 3)))
    assert(!geq(List(1, 2, 3), List(1, 2, 4)))
    assert(geq(List(unit, unit), List(unit, unit)))
    assert(!geq(List(unit), List(unit, unit)))
  }

  it should "support numbers" in {
    assert(geq(42, 42))
    assert(!geq(42, 7))
  }
  it should "support chars" in {
    assert(geq('4', '4'))
    assert(!geq('4', '2'))
  }
  it should "support strings" in {
    assert(geq("42", "42"))
    assert(!geq("42", "7"))
  }
  it should "support heterogeneous pairs" in {
    assert(geq(("42", 7), ("42", 7)))
    assert(!geq(("42", 7), ("7", 7)))
    assert(!geq(("42", 7), ("42", 42)))
  }
  it should "support homogeneous pairs" in {
    assert(geq(("42", "7"), ("42", "7")))
    assert(!geq(("42", "7"), ("7", "7")))
    assert(!geq(("42", "7"), ("42", "42")))
  }

  "folding" should "have working foldl examples" in {
    val div = (_: Int) / (_: Int)
    val max = scala.math.max(_: Int, _: Int)
    assert(foldl(div)(64)(List(4, 2, 4)) == 2)
    assert(foldl(div)(3)(List.empty: List[Int]) == 3)
    assert(foldl(max)(5)(List(1, 2, 3, 4)) == 5)
    assert(foldl(max)(5)(List(1, 2, 3, 4, 5, 6, 7)) == 7)
    assert(foldl((x: Int, y: Int) ⇒ 2 * x + y)(4)(List(1, 2, 3)) == 43)
  }

  it should "have working foldr examples" in {
    val max = scala.math.max(_: Int, _: Int)
    assert(foldr((_: Int) + (_: Int))(5)(List(1, 2, 3, 4)) == 15)
    assert(foldr((_: Int) / (_: Int))(2)(List(8, 12, 24, 4)) == 8)
    assert(foldr((_: Int) / (_: Int))(3)(List.empty: List[Int]) == 3)
    assert(foldr((_: Boolean) && (_: Boolean))(true)(List(1 > 2, 3 > 2, 5 == 5)) == false)
    assert(foldr(max)(18)(List(3, 6, 12, 4, 55, 11)) == 55)
    assert(foldr(max)(111)(List(3, 6, 12, 4, 55, 11)) == 111)
    assert(foldr((a: Int, b: Int) ⇒ (a + b) / 2)(54)(List(12, 4, 10, 6)) == 12)
  }
}
