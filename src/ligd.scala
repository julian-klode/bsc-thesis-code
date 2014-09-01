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

  def geq[A: Rep](a: A, b: A): Boolean = (rep[A], a, b) match {
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

  implicit def rSum[A: Rep, B: Rep]: Rep[Either[A, B]] = RSum(rep[A], rep[B])
  implicit def rProd[A: Rep, B: Rep]: Rep[(A, B)] = RProd(rep[A], rep[B])

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

  implicit def rList[A: Rep]: Rep[List[A]] = RList(rep[A])

  /**
   * Usage: gfoldl(fun: (A, N) => A)(unit: A)(container: C)
   *
   * A simple left fold over some container. The function needs to have both
   * argument types annotated, otherwise Scala won't be able to infer the
   * representation of N.
   *
   * Type notes: A means accumulator, C means container, N means needle
   *
   */
  def gfoldl[A, C: Rep, N: Rep](fun: (A, N) ⇒ A)(unit: A)(c: C): A = (rep[C], c) match {
    case (r, v) if r == rep[N]    ⇒ fun(unit, v.asInstanceOf[N])
    case (RSum(ra, rb), Left(x))  ⇒ gfoldl(fun)(unit)(x)(ra, rep[N])
    case (RSum(ra, rb), Right(x)) ⇒ gfoldl(fun)(unit)(x)(rb, rep[N])
    case (RProd(ra, rb), (x, y))  ⇒ gfoldl(fun)(gfoldl(fun)(unit)(x)(ra, rep[N]))(y)(rb, rep[N])
    case (r: RType[_, C], t1)     ⇒ gfoldl(fun)(unit)(r.b.from(t1))(r.a, rep[N])
    case _                        ⇒ unit
  }

  /**
   * Usage: gfoldr(fun: (A, N) => A)(unit: A)(container: C)
   *
   * A simple right fold over some container. The function needs to have both
   * argument types annotated, otherwise Scala won't be able to infer the
   * representation of N.
   *
   * Type notes: A means accumulator, C means container, N means needle
   *
   */
  def gfoldr[A, C: Rep, N: Rep](fun: (N, A) ⇒ A)(unit: A)(c: C): A = (rep[C], c) match {
    case (r, v) if r == rep[N]    ⇒ fun(v.asInstanceOf[N], unit)
    case (RSum(ra, rb), Left(x))  ⇒ gfoldr(fun)(unit)(x)(ra, rep[N])
    case (RSum(ra, rb), Right(x)) ⇒ gfoldr(fun)(unit)(x)(rb, rep[N])
    case (RProd(ra, rb), (x, y))  ⇒ gfoldr(fun)(gfoldr(fun)(unit)(y)(rb, rep[N]))(x)(ra, rep[N])
    case (r: RType[_, C], t1)     ⇒ gfoldr(fun)(unit)(r.b.from(t1))(r.a, rep[N])
    case _                        ⇒ unit
  }

  /** Simple foldl, like scala's foldLeft */
  def foldl[A, C[_], N](c: C[N])(unit: A)(fun: (A, N) ⇒ A)(implicit rep: Rep[C[N]], rn: Rep[N]): A = gfoldl(fun)(unit)(c)

  /** Simple foldr, like scala's foldRight */
  def foldr[A, C[_], N](c: C[N])(unit: A)(fun: (N, A) ⇒ A)(implicit rep: Rep[C[N]], rn: Rep[N]): A = gfoldr(fun)(unit)(c)

  /**
   * Find all instances of a given type in an object.
   *
   * @param rn The representation of objects were a looking for
   * @param c The containing object we are searching in
   * @param rc (Implicit) Representation of c
   */
  def findAll[N, C: Rep](rn: Rep[N])(c: C) = gfoldr(
    (x: N, xs: List[N]) ⇒ x :: xs
  )(List.empty)(c)(rep[C], rn)

  /**
   * A generic sum that can sum both integers and floats (and products
   *
   * @param rt The representation of T values we want to build sums of
   * @param c  The container containing (or not) or T values
   */
  def gSum[T, C: Rep](rt: Rep[T], c: C): T = {
    gfoldl(add(rt))(zero(rt))(c)(rep[C], rt)
  }

  /**
   * A generic sum that can sum both integers and floats (and products
   *
   * @param rt The representation of T values we want to build sums of
   * @param c  The container containing (or not) or T values
   */
  def sum[T, C[_]](c: C[T])(implicit rep: Rep[C[T]], rt: Rep[T]): T = {
    gfoldl(add(rt))(zero(rt))(c)(rep, rt)
  }

  /** Helper for sumOf: Add two objects of same type */
  def add[T](rep: Rep[T])(a: T, b: T): T = rep match {
    case RInt          ⇒ a + b
    case RFloat        ⇒ a + b
    case RProd(ra, rb) ⇒ (add(ra)(a._1, b._1), add(rb)(a._2, b._2))
    case _             ⇒ throw new Exception("Unknown types")
  }

  /** Helper for sumOf: Return a zero value */
  def zero[T: Rep]: T = rep[T] match {
    case RInt          ⇒ 0
    case RFloat        ⇒ 0F
    case RProd(ra, rb) ⇒ (zero(ra), zero(rb))
    case _             ⇒ throw new Exception("Unknown types")
  }

  /** Helper: If a is None, return n, otherwise the minimum of both */
  def _minOrNone(a: Option[Int], n: Int): Option[Int] = Some(a.fold(n)(scala.math.min(_, n)))
  /** Find the minimum Integer in an object. If none exists, return None */
  def gMinInt[C: Rep](c: C): Option[Int] = gfoldl(_minOrNone)(None)(c)
  /** Find the minimum integer in a container of integers */
  def minInt[C[_]](c: C[Int])(implicit rep: Rep[C[Int]]): Option[Int] = gMinInt(c)(rep)

  /** A small helper to get the implicit representation of a type */
  def rep[T: Rep] = implicitly[Rep[T]]

  /**
   * Apply a transformation to all objects of N in c.
   *
   * This is similar to everywhere in SYB or Uniplate. And nope, our Haskell
   * pals can't do this with their LIGD.
   *
   * @param fun Transformation to apply
   * @param c Object to apply to
   */
  def everywhere[C: Rep, N: Rep](fun: N ⇒ N)(c: C): C = (rep[C], c) match {
    case (r, v) if r == rep[N]    ⇒ fun(v.asInstanceOf[N]).asInstanceOf[C]
    case (RSum(ra, rb), Left(x))  ⇒ Left(everywhere(fun)(x)(ra, rep[N]))
    case (RSum(ra, rb), Right(x)) ⇒ Right(everywhere(fun)(x)(rb, rep[N]))
    case (RProd(ra, rb), (x, y))  ⇒ (everywhere(fun)(x)(ra, rep[N]), everywhere(fun)(y)(rb, rep[N]))
    case (r: RType[_, C], t1)     ⇒ r.b.to(everywhere(fun)(r.b.from(t1))(r.a, rep[N]))
    case (r, v)                   ⇒ v
  }

  /**
   * A function that returns a T and accepts any representable value.
   *
   * This represents the type of a first class generic function. This is
   * the same type as: (forall u. Rep u => u -> t) in Haskell.
   */
  trait Returning[T] {
    def apply[U: Rep](u: U): T
  }

  /**
   * A generic show that we can pass around
   */
  object gshow extends Returning[String] {
    override def apply[U: Rep](u: U): String = (rep[U], u) match {
      case (RInt, i)                ⇒ i.toString()
      case (RFloat, i)              ⇒ i.toString()
      case (RBoolean, i)            ⇒ i.toString()
      case (RString, i)             ⇒ i.toString()
      case (RChar, i)               ⇒ i.toString()
      case (RUnit, i)               ⇒ "()"
      case (RSum(ra, rb), Left(x))  ⇒ "Left(" + gshow(x)(ra) + ")"
      case (RSum(ra, rb), Right(x)) ⇒ "Right(" + gshow(x)(rb) + ")"
      case (RProd(ra, rb), (a, b))  ⇒ "(" + gshow(a)(ra) + ", " + gshow(b)(rb) + ")"
      case (RList(ra), Nil)         ⇒ "Nil"
      case (RList(ra), x :: xs)     ⇒ gshow(x)(ra) + " :: " + gshow(xs)
      case (r: RType[_, U], t1)     ⇒ gshow(r.b.from(t1))(r.a)
      case (r, v)                   ⇒ throw new Exception("Should not happen")
    }
  }

  /**
   * Implementation of gmapQ, as known from SYB.
   *
   * This is a silly version, it only supports constructors of arrity 2. But
   * it's enough to work on lists and show that generic functions can be used
   * as first class arguments.
   */
  def gmapQ[C: Rep, T](fun: Returning[T])(c: C): List[T] = (rep[C], c) match {
    case (RSum(ra, rb), Left(x))  ⇒ gmapQ(fun)(x)(ra)
    case (RSum(ra, rb), Right(x)) ⇒ gmapQ(fun)(x)(rb)
    case (RProd(ra, rb), (x, y))  ⇒ List(fun(x)(ra), fun(y)(rb))
    case (r: RType[_, C], t1)     ⇒ gmapQ(fun)(r.b.from(t1))(r.a)
    case (r, v)                   ⇒ List(fun(v)(r))
  }

  /**
   * Simple straight forward minimum.
   *
   * Returns Int.MaxValue if no smaller integer is found.
   */
  def min[C: Rep](c: C): Int = (rep[C], c) match {
    case (RSum(ra, rb), Left(x))  ⇒ min(x)(ra)
    case (RSum(ra, rb), Right(x)) ⇒ min(x)(rb)
    case (RProd(ra, rb), (x, y))  ⇒ scala.math.min(min(x)(ra), min(y)(rb))
    case (r: RType[_, C], t1)     ⇒ min(r.b.from(t1))(r.a)
    case (RInt, i)                ⇒ i
    case _                        ⇒ Int.MaxValue
  }
}
