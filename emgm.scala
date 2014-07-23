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

/** I'm not sure implementing EMGM is possible in Scala.
  *
  * The whole thing seems to fail because we cannot have static methods
  * in a trait. So we cannot access representations of sum or product
  * elements.
  */
object EMGM {

  // Units, sums and products
  type Unit = Unit.type
  case object Unit
  sealed abstract class Sum[A, B]
  sealed case class Inl[A, B](val a: A) extends Sum[A, B]
  sealed case class Inr[A, B](val b: B) extends Sum[A, B]
  sealed case class Prod[A, B](val a: A, val b: B)
  /** Isomorphism for converting between types */
  sealed case class Iso[B, C](val from: B ⇒ C, val to: C ⇒ B)

  /** Representation of generic functions.
    *
    * This gets ugly, because G is a type constructor.
    */
  trait Generic[G[_]] {
    def unit: G[Unit]
    def plus[A, B](a: G[A], b: G[B]): G[Sum[A, B]]
    def prod[A, B](a: G[A], b: G[B]): G[Prod[A, B]]
    def constr[A](name: Symbol, arity: Int, arg: G[A]): G[A] = arg
    def char: G[Char]
    def int: G[Int]
    def view[A, B](iso: Iso[B, A], a: G[A]): G[B]
  }

  /* ====================================================================
   *          EXAMPLE: Encoding data to bits
   * ====================================================================
   */
  def const[A, B](a: A)(b: B) = a
  case class Encode[A](encodeS: A ⇒ List[Boolean]) extends Generic[Encode] {
    override def unit = Encode(const(List()))
    override def plus[A, B](a: Encode[A], b: Encode[B]) = Encode((x: Sum[A, B]) ⇒ x match {
      case Inl(l) ⇒ false :: a.encodeS(l)
      case Inr(r) ⇒ true :: b.encodeS(r)
    })
    override def prod[A, B](a: Encode[A], b: Encode[B]) = Encode((x: Prod[A, B]) ⇒ a.encodeS(x.a) ++ b.encodeS(x.b))

    override def char = Encode(encodeChar)
    override def int = Encode(encodeInt)
    override def view[A, B](iso: Iso[B, A], a: Encode[A]) = Encode((x: B) ⇒ a.encodeS(iso.from(x)))
  }
  val encodeG = Encode(const(List()))

  /* Stubs */
  def encodeInt(i: Int) = List(true)
  def encodeChar(c: Char) = List(false)

  /* ====================================================================
   *          EXAMPLE: LISTS
   * ====================================================================
   */
  def isoList[A]: Iso[List[A], Sum[Unit, Prod[A, List[A]]]] = Iso(fromList, toList)
  def fromList[A](list: List[A]): Sum[Unit, Prod[A, List[A]]] = list match {
    case Nil       ⇒ Inl(Unit)
    case (a :: as) ⇒ Inr(Prod(a, as))
  }

  def toList[A](list: Sum[Unit, Prod[A, List[A]]]): List[A] = list match {
    case Inl(Unit)        ⇒ List.empty
    case Inr(Prod(a, as)) ⇒ a :: as
  }

  def rList[G[_], A, B](g: Generic[G])(a: G[A]): G[List[A]] = g.view(isoList, g.plus(g.unit, g.prod(a, rList(g)(a))))

  /** It seems we need to translate Rep like this, but:
    *
    * We cannot implement Product or Sums this way as we do not have access
    * to the Reps of the left and right sides.
    */
  trait Rep[A] {
    def rep[G[_]](g: Generic[G]): G[A]
  }

  /** Let's try implementing Rep like this:
    *
    * Does not work either. While we can implement the product case now,
    * there's no way for us to implement Sum, because we can only access
    * either the left or the right side, and thus, only their representations.
    */
  trait Rep1 {
    def rep1[G[_]](g: Generic[G]): G[this.type]
  }

  implicit case object RUnit extends Rep[Unit] {
    override def rep[G[_]](g: Generic[G]): G[Unit] = g.unit
  }
  implicit case object RChar extends Rep[Char] {
    override def rep[G[_]](g: Generic[G]): G[Char] = g.char
  }
  implicit case object RInt extends Rep[Int] {
    override def rep[G[_]](g: Generic[G]): G[Int] = g.int
  }

  /*
   * We'd need to access a Rep[A] and a Rep[B] for this to work. How are
   * we going to do this?
   *
  case class RSum[A, B]() extends Rep[Sum[A, B]] {
    override def rep[G[_]](g: Generic[G]): G[Sum[A, B]] = g.plus(rep(g): G[A], rep(g): G[B])
  }
  */
  def encode[T](t: T)(implicit r: Rep[T]): List[Boolean] = r.rep(encodeG).encodeS(t)

  val b = encodeG.prod(encodeG.int, encodeG.char).encodeS(Prod(1, 'x'))

}
