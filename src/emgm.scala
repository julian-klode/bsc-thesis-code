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

import scala.language.higherKinds
import scala.language.reflectiveCalls

/**
 * Implementation of (Extensible and Modular) Generics for the Masses
 *
 * Based on: Extensible and Modular Generics for the Masses
 * by: Bruno C. d. S. Oliveira , Ralf Hinze, Andres L
 *
 * Based on: Scala for Generic Programmers
 * by: Bruno C. d. S. Oliveira and Jeremy Gibbons
 */
trait EMGM {

  /** Isomorphism for converting between types */
  sealed case class Iso[B, C](val from: B ⇒ C, val to: C ⇒ B)

  /**
   * Representation of generic functions.
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
    def float: G[Float]
    def string: G[String]
    def view[A, B]: Iso[B, A] ⇒ (⇒ G[A]) ⇒ G[B]
  }

  /**
   * Representation of types.
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
  implicit def RSum[A, B](implicit a: Rep[A], b: Rep[B]) = new Rep[Either[A, B]] {
    override def rep[G[_]](implicit g: Generic[G]): G[Either[A, B]] = g.plus(a.rep)(b.rep)
  }
  implicit def RProd[A, B](implicit a: Rep[A], b: Rep[B]) = new Rep[(A, B)] {
    override def rep[G[_]](implicit g: Generic[G]): G[(A, B)] = g.prod(a.rep)(b.rep)
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
  implicit def RFloat = new Rep[Float] {
    override def rep[G[_]](implicit g: Generic[G]): G[Float] = g.float
  }

  /* ====================================================================
   *         EXAMPLE: Encoding data to bits
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
    override def float = Encode(encodeFloat)
    override def string = Encode(encodeString)
    override def view[A, B] = iso ⇒ a ⇒ Encode(x ⇒ a.encodeS(iso.from(x)))
  }

  /* Stubs */
  def encodeInt(i: Int) = List(true)
  def encodeFloat(i: Float) = List(true)
  def encodeChar(c: Char) = List(false)
  def encodeString(c: String) = List(false)

  /* Generic function */
  def encode[T](t: T)(implicit r: Rep[T]): List[Boolean] = r.rep(MyEncode).encodeS(t)

  /* ====================================================================
   *         EXAMPLE: LISTS
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
   *         EXAMPLE: Function1s
   * ====================================================================
   */

  case class Sum[A](sum: A ⇒ Int)
  class MySum extends Generic[Sum] {
    override def unit = Sum(_ ⇒ 0)
    override def plus[A, B] = a ⇒ b ⇒ Sum(_ match {
      case Left(l)  ⇒ a.sum(l)
      case Right(r) ⇒ b.sum(r)
    })
    override def prod[A, B] = a ⇒ b ⇒ Sum(x ⇒ a.sum(x._1) + b.sum(x._2))
    override def char = Sum(_ ⇒ 0)
    override def int = Sum((x: Int) ⇒ x)
    override def float = Sum(_ ⇒ 0)
    override def string = Sum(_ ⇒ 0)
    override def view[A, B] = iso ⇒ a ⇒ Sum(x ⇒ a.sum(iso.from(x)))
  }
  class MyCountInt extends MySum {
    override def int = Sum(x ⇒ 1)
  }

  /* Generic function */
  def sum[T](t: T)(implicit r: Rep[T]): Int = r.rep(new MySum).sum(t)
  def countInt[T](t: T)(implicit r: Rep[T]): Int = r.rep(new MyCountInt).sum(t)

  /*
   * EXAMPLE: Generic equality
   */

  case class GEq[A](geq: A ⇒ A ⇒ Boolean)

  class MyGEq extends Generic[GEq] {
    override def unit = GEq(x ⇒ y ⇒ true)
    override def plus[A, B] = a ⇒ b ⇒ GEq(x ⇒ y ⇒ (x, y) match {
      case (Left(x), Left(y))   ⇒ a.geq(x)(y)
      case (Right(x), Right(y)) ⇒ b.geq(x)(y)
      case (_, _)               ⇒ false
    })
    override def prod[A, B] = a ⇒ b ⇒ GEq(x ⇒ y ⇒
      a.geq(x._1)(y._1) && b.geq(x._2)(y._2)
    )
    override def char = GEq(x ⇒ y ⇒ x == y)
    override def int = GEq(x ⇒ y ⇒ x == y)
    override def float = GEq(x ⇒ y ⇒ x == y)
    override def string = GEq(x ⇒ y ⇒ x == y)
    override def view[A, B] = iso ⇒ a ⇒ GEq(x ⇒ y ⇒ a.geq(iso.from(x))(iso.from(y)))
  }

  def geq[T](a: T, b: T)(implicit r: Rep[T]): Boolean = r.rep(new MyGEq).geq(a)(b)
}

// Oliveira, Hinze & Loeh, sec. 1.5
trait EMGM_sec_1_5 extends EMGM {

  // the type parameter G is instantiated to the operation.
  // In the GEq example, G is instantiated to GEq.
  // see `implicit object GEqList`
  trait GRep[G[_], A] {
    def grep: G[A]
  }

  // since the generic operation G has to be fixed before a subclass of GRep
  // is instantiated, subclasses must take `g: Generic[G]` as a constructor
  // parameter.
  implicit def GRString[G[_]](implicit g: Generic[G]) = new GRep[G, String] {
    def grep: G[String] = g.string
  }

  implicit def GRUnit[G[_]](implicit g: Generic[G]) = new GRep[G, Unit] {
    def grep: G[Unit] = g.unit
  }

  implicit def GRChar[G[_]](implicit g: Generic[G]) = new GRep[G, Char] {
    def grep: G[Char] = g.char
  }

  implicit def GRInt[G[_]](implicit g: Generic[G]) = new GRep[G, Int] {
    def grep: G[Int] = g.int
  }

  implicit def GRFloat[G[_]](implicit g: Generic[G]) = new GRep[G, Float] {
    def grep: G[Float] = g.float
  }

  implicit def GRSum[G[_], A, B](implicit g: Generic[G], a: GRep[G, A], b: GRep[G, B]) =
    new GRep[G, Either[A, B]] {
      override def grep: G[Either[A, B]] = g.plus(a.grep)(b.grep)
    }

  implicit def GRProd[G[_], A, B](implicit g: Generic[G], a: GRep[G, A], b: GRep[G, B]) =
    new GRep[G, (A, B)] {
      override def grep: G[(A, B)] = g.prod(a.grep)(b.grep)
    }

  // generic operation extended with an extra case for lists
  trait GenericList[G[_]] extends Generic[G] {
    def list[A]: G[A] ⇒ G[List[A]] = a ⇒ rList(a)(this)
  }

  // dispatcher for the extra `list` case
  implicit def GRList[G[_], A](implicit g: GenericList[G], a: GRep[G, A]) =
    new GRep[G, List[A]] {
      def grep: G[List[A]] = g.list(a.grep)
    }

  // generic equality with a special case for lists
  // should be passed to `GRList` as the implicit parameter `g: GenericList[G]`,
  // hence declared as implicit
  implicit object GEqList extends MyGEq with GenericList[GEq] {

    // special case of GEq for lists, semantically equivalent
    // to an object of type MyGeq. It is here to demonstrate
    // that it is possible to add new cases to a generic operation.
    //
    // 1. If two lists have unequal length, then they are not equal.
    // 2. Two empty lists are equal.
    // 3. Two nonempty lists of equal length are equal if all their
    //    elements are equal.
    override def list[A]: GEq[A] ⇒ GEq[List[A]] = eq ⇒ GEq { xs ⇒
      ys ⇒
        xs.length == ys.length && (
          xs.isEmpty ||
          xs.zip(ys).map({ case (x, y) ⇒ eq.geq(x)(y) }).min)
    }
  }

  def geqList[T](a: T, b: T)(implicit r: GRep[GEq, T]): Boolean =
    r.grep.geq(a)(b)

  /* Calculation of a sum */
  case class GSum[N](gsum: N ⇒ Int ⇒ Int)
  implicit object MySum extends GenericList[GSum] {
    override def unit = GSum(n ⇒ r ⇒ r)
    override def plus[A, B] = a ⇒ b ⇒ GSum(x ⇒ r ⇒ x match {
      case (Left(v))  ⇒ a.gsum(v)(r)
      case (Right(v)) ⇒ b.gsum(v)(r)
    })

    override def prod[A, B] = a ⇒ b ⇒ GSum(x ⇒ r ⇒ b.gsum(x._2)(a.gsum(x._1)(r)))

    override def char = GSum(x ⇒ r ⇒ r)
    override def int = GSum(x ⇒ r ⇒ x + r)
    override def float = GSum(x ⇒ r ⇒ r)
    override def string = GSum(x ⇒ r ⇒ r)
    override def view[A, B] = iso ⇒ a ⇒ GSum(x ⇒ r ⇒ a.gsum(iso.from(x))(r))
  }

  def sum[C](a: C)(implicit r: GRep[GSum, C]): Int = {
    r.grep.gsum(a)(0)
  }

  /* Calculation of a minimum */
  case class GMin[N](gmin: N ⇒ Int ⇒ Int)
  implicit object MyMin extends GenericList[GMin] {
    override def unit = GMin(n ⇒ r ⇒ r)
    override def plus[A, B] = a ⇒ b ⇒ GMin(x ⇒ r ⇒ x match {
      case (Left(v))  ⇒ a.gmin(v)(r)
      case (Right(v)) ⇒ b.gmin(v)(r)
    })

    override def prod[A, B] = a ⇒ b ⇒ GMin(x ⇒ r ⇒ b.gmin(x._2)(a.gmin(x._1)(r)))

    override def char = GMin(x ⇒ r ⇒ r)
    override def int = GMin(x ⇒ r ⇒ scala.math.min(x, r))
    override def float = GMin(x ⇒ r ⇒ r)
    override def string = GMin(x ⇒ r ⇒ r)
    override def view[A, B] = iso ⇒ a ⇒ GMin(x ⇒ r ⇒ a.gmin(iso.from(x))(r))
  }

  def min[C](a: C)(implicit r: GRep[GMin, C]): Int = {
    r.grep.gmin(a)(Int.MaxValue)
  }

  /*
   * Generic folding
   *
   * This does not work obviously. The idea would be (IMO) to subclass the
   * folding trait for every specific operation. But this won't work well,
   * it seems.
   */
  trait Apply1Of2[T[_, _], A] {
    type Apply[B] = T[A, B]
  }

  case class GFold[R, N](gfold: N ⇒ R ⇒ R)
  trait MyFold[R] extends GenericList[Apply1Of2[GFold, R]#Apply] {
    override def unit = GFold(n ⇒ r ⇒ r)
    override def plus[A, B] = a ⇒ b ⇒ GFold(x ⇒ r ⇒ x match {
      case (Left(v))  ⇒ a.gfold(v)(r)
      case (Right(v)) ⇒ b.gfold(v)(r)
    })

    override def prod[A, B] = a ⇒ b ⇒ GFold(x ⇒ r ⇒ b.gfold(x._2)(a.gfold(x._1)(r)))

    override def char = GFold(x ⇒ r ⇒ r)
    override def int = GFold(x ⇒ r ⇒ r)
    override def float = GFold(x ⇒ r ⇒ r)
    override def string = GFold(x ⇒ r ⇒ r)
    override def view[A, B] = iso ⇒ a ⇒ GFold(x ⇒ r ⇒ a.gfold(iso.from(x))(r))
  }

  // demonstrating EMGM as a solution to the "expression problem"
  // would be interesting. see Oliveira, Hinze & Loeh, sec. 1.6.1

  /*
   * Higher-order functions:
   *
   * The thing is: We would not actually need to do this, we could simply use
   * the GRep directly instead. But in practice, the method names are different
   * in each GRep, and the wrapper functions might actually have some extra
   * arguments they pass to those methods, like min does with Int.MaxValue.
   *
   * If we make the wrapper functions objects conforming to this type, things
   * work just fine
   */
  type Returning[F[_], R] = {
    def apply[C](a: C)(implicit r: GRep[F, C]): R
  }

  def apply[F[_],C,R](fun: Returning[F, R])(c: C)(implicit r: GRep[F, C]): R = fun(c)
}

object EMGM extends EMGM with EMGM_sec_1_5
