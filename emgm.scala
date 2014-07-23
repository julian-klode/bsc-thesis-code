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
    def plus[A, B](a: G[A], b: G[B]): G[Either[A, B]]
    def prod[A, B](a: G[A], b: G[B]): G[(A, B)]
    def constr[A](name: Symbol, arity: Int, arg: G[A]): G[A] = arg
    def char: G[Char]
    def int: G[Int]
    def view[A, B](iso: Iso[B, A], a: ⇒ G[A]): G[B]
  }

  /** Representation of types.
    *
    * This gets ugly because we need to pass implicit Rep[A] objects around
    * for every object A.
    */
  trait Rep[A] {
    def rep[G[_]](g: Generic[G]): G[A]
  }

  implicit def RUnit = new Rep[Unit] {
    override def rep[G[_]](g: Generic[G]): G[Unit] = g.unit
  }
  implicit def RChar = new Rep[Char] {
    override def rep[G[_]](g: Generic[G]): G[Char] = g.char
  }
  implicit def RInt = new Rep[Int] {
    override def rep[G[_]](g: Generic[G]): G[Int] = g.int
  }

  /** Thanks to Scala for Generic Programmers :). I did not figure out how to
    * get the Rep[A] and Rep[B] stuff done, I tried to use case classes for
    * implementing the Rep, which failed :(
    */
  implicit def REither[A, B](implicit a: Rep[A], b: Rep[B]) = new Rep[Either[A, B]] {
    override def rep[G[_]](g: Generic[G]): G[Either[A, B]] = g.plus(a.rep(g), b.rep(g))
  }
  implicit def RTuple2[A, B](implicit a: Rep[A], b: Rep[B]) = new Rep[(A, B)] {
    override def rep[G[_]](g: Generic[G]): G[(A, B)] = g.prod(a.rep(g), b.rep(g))
  }

  object Examples {

    /* ====================================================================
   *          EXAMPLE: Encoding data to bits
   * ====================================================================
   */
    def const[A, B](a: A)(b: B) = a
    case class Encode[A](encodeS: A ⇒ List[Boolean]) extends Generic[Encode] {
      override def unit = Encode(const(List()))
      override def plus[A, B](a: Encode[A], b: Encode[B]) = Encode((x: Either[A, B]) ⇒ x match {
        case Left(l)  ⇒ false :: a.encodeS(l)
        case Right(r) ⇒ true :: b.encodeS(r)
      })
      override def prod[A, B](a: Encode[A], b: Encode[B]) = Encode((x: (A, B)) ⇒ a.encodeS(x._1) ++ b.encodeS(x._2))

      override def char = Encode(encodeChar)
      override def int = Encode(encodeInt)
      override def view[A, B](iso: Iso[B, A], a: ⇒ Encode[A]) = Encode((x: B) ⇒ a.encodeS(iso.from(x)))
    }
    val encodeG = Encode(const(List()))

    /* Stubs */
    def encodeInt(i: Int) = List(true)
    def encodeChar(c: Char) = List(false)

    /* Generic function */
    def encode[T](t: T)(implicit r: Rep[T]): List[Boolean] = r.rep(encodeG).encodeS(t)

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
      view(isoList, plus(unit, prod(a, rList(a))))
    }

    implicit def RList[A](implicit a: Rep[A]) = new Rep[List[A]] {
      override def rep[G[_]](gen: Generic[G]) =
        rList(a.rep(gen))(gen)
    }
  }
}
