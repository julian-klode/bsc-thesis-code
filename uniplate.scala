/*
 * uniplate.scala - Implementation of Uniplate for Scala.
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

/** Implementation of Uniplate for Scala
  *
  * Based on:
  * Uniform Boilerplate and List Processing
  * Or: Scrap Your Scary Types
  * by:
  * Neil Mitchell, Colin Runciman
  * Published at:
  * http://community.haskell.org/~ndm/downloads/paper-uniform_boilerplate_and_list_processing-30_sep_2007.pdf
  */
object UniPlate {

  /** The uniplate type class
    *
    * We are implementing the other functions here too, marked as final because
    * it's easier to do here than outside where implicit parameters are used.
    */
  trait Uniplate[T] {
    def uniplate(self: T): (List[T], List[T] ⇒ T)

    /* Implementation of additional functions */
    private[UniPlate] final def children(self: T): List[T] = uniplate(self)._1
    private[UniPlate] final def universe(self: T): List[T] = self :: children(self).flatMap(universe)
    private[UniPlate] final def transform(f: T ⇒ T)(self: T): T = {
      val (children, context) = uniplate(self)
      f(context(children.map(transform(f))))
    }
    private[UniPlate] final def descend(f: T ⇒ T)(self: T): T = {
      val (children, context) = uniplate(self)
      context(children.map(f))
    }
    private[UniPlate] final def rewrite(f: T ⇒ Option[T])(self: T): T = {
      def g(self: T) = f(self).map(rewrite(f)) getOrElse self
      transform(g)(self)
    }
  }

  implicit object UniplateInt extends Uniplate[Int] {
    def uniplate(self: Int) = (List.empty, _ ⇒ self)
  }
  implicit object UniplateFloat extends Uniplate[Float] {
    def uniplate(self: Float) = (List.empty, _ ⇒ self)
  }
  implicit object UniplateString extends Uniplate[String] {
    def uniplate(self: String) = (List.empty, _ ⇒ self)
  }
  implicit object UniplateChar extends Uniplate[Char] {
    def uniplate(self: Char) = (List.empty, _ ⇒ self)
  }

  /** Get all children and the creator function */
  def uniplate[T](self: T)(implicit up: Uniplate[T]) = up.uniplate(self)
  /** Get all direct children of the same type */
  def children[T](self: T)(implicit up: Uniplate[T]) = up.uniplate(self)._1
  /** Get all children of the same type, recursively */
  def universe[T](self: T)(implicit up: Uniplate[T]) = up.universe(self)
  /** Apply a transformation recursively to all children of type T */
  def transform[T](f: T ⇒ T)(self: T)(implicit up: Uniplate[T]) = up.transform(f)(self)
  /** Like transform, but non-recursively */
  def descend[T](f: T ⇒ T)(self: T)(implicit up: Uniplate[T]) = up.descend(f)(self)
  /** Apply a transformation repeatedly, until a normal form is found */
  def rewrite[T](f: T ⇒ Option[T])(self: T)(implicit up: Uniplate[T]) = up.rewrite(f)(self)

}

class UniplateTest extends FlatSpec {
  import UniPlate._

  sealed abstract class Expr
  case class Var(s: String) extends Expr
  case class Val(i: Int) extends Expr
  case class Add(a: Expr, b: Expr) extends Expr
  case class Mul(a: Expr, b: Expr) extends Expr

  implicit object UniplateExpr extends Uniplate[Expr] {
    def uniplate(self: Expr) = self match {
      case Add(a, b) ⇒ (List(a, b), { case List(a, b) ⇒ Add(a, b) })
      case Mul(a, b) ⇒ (List(a, b), { case List(a, b) ⇒ Mul(a, b) })
      case _         ⇒ (List.empty, _ ⇒ self)
    }
  }

  val child11 = Var("x")
  val child12 = Val(5)
  val child1 = Mul(child11, child12)
  val child2 = Val(1)
  val exp: Expr = Add(child1, child2)

  "children" should "work" in {
    assert(children(exp) == List(child1, child2))
    assert(universe(exp) == List(exp, child1, child11, child12, child2))
  }

  "transform" should "replace x with y" in {
    val child11_ = Var("y")
    val child1_ = Mul(child11_, child12)
    val exp_ : Expr = Add(child1_, child2)
    def f(e: Expr) = e match {
      case Var("x") ⇒ child11_
      case x        ⇒ x
    }

    assert(transform(f)(exp) == exp_);
  }
  "rewrite" should "reduce expressions" in {
    val x = Var("x")
    val exp: Expr = Mul(Add(Add(x, x), Add(x, x)), Val(3))
    val exp_ : Expr = Mul(Val(12), x)
    def f(e: Expr) = e match {
      case Add(Val(x), Val(y))         ⇒ Some(Val(x + y))
      case Mul(Val(x), Val(y))         ⇒ Some(Val(x * y))
      case Add(x, y) if x == y         ⇒ Some(Mul(Val(2), y))
      case Mul(Val(x), Mul(Val(y), z)) ⇒ Some(Mul(Val(x * y), z))
      case Mul(Val(x), Mul(z, Val(y))) ⇒ Some(Mul(Val(x * y), z))
      case Mul(Mul(Val(y), z), Val(x)) ⇒ Some(Mul(Val(x * y), z))
      case Mul(Mul(z, Val(y)), Val(x)) ⇒ Some(Mul(Val(x * y), z))
      case default                     ⇒ None
    }

    assert(rewrite[Expr](f)(exp) == exp_);
  }

}
