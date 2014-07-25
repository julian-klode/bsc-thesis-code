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

import org.scalatest._

/** Implementation of (Extensible and Modular) Generics for the Masses
  *
  * Based on: Extensible and Modular Generics for the Masses
  * by: Bruno C. d. S. Oliveira , Ralf Hinze, Andres L
  *
  * Based on: Scala for Generic Programmers
  * by: Bruno C. d. S. Oliveira and Jeremy Gibbons
  */
object EMGM {
  /** Isomorphism for converting between types */
  sealed case class Iso[B, C](val from: B ⇒ C, val to: C ⇒ B)

  /** Representation of generic functions.
    *
    * This gets ugly, because G is a type constructor.
    */
  trait Generic[G[_]] {
    def unit: G[Unit]
    def plus[A, B]: G[A] ⇒ G[B] ⇒ G[Either[A, B]]
    def prod[A, B]: G[A] ⇒ G[B] ⇒ G[(A, B)]
    def constr[A]: Symbol ⇒ Int ⇒ G[A] ⇒ G[A] = (name ⇒ arity ⇒ arg ⇒ arg)
    def char: G[Char]
    def int: G[Int]
    def string: G[String]
    def view[A, B]: Iso[B, A] ⇒ (⇒ G[A]) ⇒ G[B]
  }

  /** Representation of types.
    *
    * This gets ugly because we need to pass implicit Rep[A] objects around
    * for every object A.
    */
  trait Rep[A] {
    def rep[G[_]](implicit g: Generic[G]): G[A]
  }

  implicit def RString = new Rep[String] {
    override def rep[G[_]](implicit g: Generic[G]): G[String] = g.string
  }
  implicit def RUnit = new Rep[Unit] {
    override def rep[G[_]](implicit g: Generic[G]): G[Unit] = g.unit
  }
  implicit def RChar = new Rep[Char] {
    override def rep[G[_]](implicit g: Generic[G]): G[Char] = g.char
  }
  implicit def RInt = new Rep[Int] {
    override def rep[G[_]](implicit g: Generic[G]): G[Int] = g.int
  }

  /** Thanks to Scala for Generic Programmers :). I did not figure out how to
    * get the Rep[A] and Rep[B] stuff done, I tried to use case classes for
    * implementing the Rep, which failed :(
    */
  implicit def REither[A, B](implicit a: Rep[A], b: Rep[B]) = new Rep[Either[A, B]] {
    override def rep[G[_]](implicit g: Generic[G]): G[Either[A, B]] = g.plus(a.rep)(b.rep)
  }
  implicit def RTuple2[A, B](implicit a: Rep[A], b: Rep[B]) = new Rep[(A, B)] {
    override def rep[G[_]](implicit g: Generic[G]): G[(A, B)] = g.prod(a.rep)(b.rep)
  }

  /* ====================================================================
   *          EXAMPLE: Encoding data to bits
   * ====================================================================
   */
  def const[A, B](a: A)(b: B) = a
  case class Encode[A](encodeS: A ⇒ List[Boolean])

  object MyEncode extends Generic[Encode] {
    override def unit = Encode(const(List()))
    override def plus[A, B] = a ⇒ b ⇒ Encode(_ match {
      case Left(l)  ⇒ false :: a.encodeS(l)
      case Right(r) ⇒ true :: b.encodeS(r)
    })
    override def prod[A, B] = a ⇒ b ⇒ Encode(x ⇒ a.encodeS(x._1) ++ b.encodeS(x._2))

    override def char = Encode(encodeChar)
    override def int = Encode(encodeInt)
    override def string = Encode(encodeString)
    override def view[A, B] = iso ⇒ a ⇒ Encode(x ⇒ a.encodeS(iso.from(x)))
  }

  /* Stubs */
  def encodeInt(i: Int) = List(true)
  def encodeChar(c: Char) = List(false)
  def encodeString(c: String) = List(false)

  /* Generic function */
  def encode[T](t: T)(implicit r: Rep[T]): List[Boolean] = r.rep(MyEncode).encodeS(t)

  /* ====================================================================
   *          EXAMPLE: LISTS
   * ====================================================================
   */
  def isoList[A]: Iso[List[A], Either[Unit, (A, List[A])]] = Iso(fromList, toList)
  def fromList[A](list: List[A]): Either[Unit, (A, List[A])] = list match {
    case Nil       ⇒ Left(Unit)
    case (a :: as) ⇒ Right((a, as))
  }

  def toList[A](list: Either[Unit, (A, List[A])]): List[A] = list match {
    case Left(())       ⇒ List.empty
    case Right((a, as)) ⇒ a :: as
  }

  def rList[G[_], A](a: G[A])(implicit g: Generic[G]): G[List[A]] = {
    import g._
    view(isoList[A])(plus(unit)(prod(a)(rList(a))))
  }

  implicit def RList[A](implicit a: Rep[A]) = new Rep[List[A]] {
    override def rep[G[_]](implicit gen: Generic[G]) =
      rList(a.rep)(gen)
  }

  /** Function1 */
  /* ====================================================================
   *          EXAMPLE: Function1s
   * ====================================================================
   */

  case class Sum[A](encodeS: A ⇒ Int)
  class MySum extends Generic[Sum] {
    override def unit = Sum(_ ⇒ 0)
    override def plus[A, B] = a ⇒ b ⇒ Sum(_ match {
      case Left(l)  ⇒ a.encodeS(l)
      case Right(r) ⇒ b.encodeS(r)
    })
    override def prod[A, B] = a ⇒ b ⇒ Sum(x ⇒ a.encodeS(x._1) + b.encodeS(x._2))
    override def char = Sum(_ ⇒ 0)
    override def int = Sum((x: Int) ⇒ x)
    override def string = Sum(_ ⇒ 0)
    override def view[A, B] = iso ⇒ a ⇒ Sum(x ⇒ a.encodeS(iso.from(x)))
  }
  class MyCountInt extends MySum {
    override def int = Sum(x ⇒ 1)
  }

  /* Generic function */
  def sum[T](t: T)(implicit r: Rep[T]): Int = r.rep(new MySum).encodeS(t)
  def countInt[T](t: T)(implicit r: Rep[T]): Int = r.rep(new MyCountInt).encodeS(t)

  /*
   * EXAMPLE: Generic equality
   */

  case class GEq[A](equals: A ⇒ A ⇒ Boolean)

  class MyGEq extends Generic[GEq] {
    override def unit = GEq(x ⇒ y ⇒ true)
    override def plus[A, B] = a ⇒ b ⇒ GEq(x ⇒ y ⇒ (x, y) match {
      case (Left(x), Left(y))   ⇒ a.equals(x)(y)
      case (Right(x), Right(y)) ⇒ b.equals(x)(y)
      case (_, _)               ⇒ false
    })
    override def prod[A, B] = a ⇒ b ⇒ GEq(x ⇒ y ⇒
      a.equals(x._1)(y._1) && b.equals(x._2)(y._2)
    )
    override def char = GEq(x ⇒ y ⇒ x == y)
    override def int = GEq(x ⇒ y ⇒ x == y)
    override def string = GEq(x ⇒ y ⇒ x == y)
    override def view[A, B] = iso ⇒ a ⇒ GEq(x ⇒ y ⇒ a.equals(iso.from(x))(iso.from(y)))
  }

  def geq[T](a: T, b: T)(implicit r: Rep[T]): Boolean = r.rep(new MyGEq).equals(a)(b)
}

class EMGMTests extends FlatSpec {
  import EMGM._
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
