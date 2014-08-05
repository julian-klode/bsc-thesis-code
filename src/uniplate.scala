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

/**
 * Implementation of Uniplate for Scala
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

  /**
   * The uniplate type class
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

  type BiplateType[B, A] = B ⇒ (List[A], List[A] ⇒ B)

  def universeOn[B, A](biplate: BiplateType[B, A])(self: B)(implicit up: Uniplate[A]) = biplate(self)._1.flatMap(universe(_)(up))
  def transformOn[B, A](biplate: BiplateType[B, A])(f: A ⇒ A)(self: B)(implicit up: Uniplate[A]) = {
    val (children, context) = biplate(self)
    context(children.map(transform(f)))
  }

  /**
   * We are encoding class Uniplate a => Biplate b a
   *
   * Because we need to encode the "Uniplate a" context, we use a class here
   * that stores an implicit Uniplate[A] object instead of making the Uniplate[A]
   * context part of the operations.
   */
  abstract class Biplate[B, A](implicit up: Uniplate[A]) {
    def biplate(self: B): (List[A], List[A] ⇒ B)
    private[UniPlate] final def universeBi(self: B) = universeOn(biplate)(self)
    private[UniPlate] final def transformBi(f: A ⇒ A)(self: B) = transformOn(biplate)(f)(self)
  }

  def biplate[B, A](self: B)(implicit bp: Biplate[B, A]) = bp.biplate(self)
  def universeBi[B, A](self: B)(implicit bp: Biplate[B, A]) = bp.universeBi(self)
  def transformBi[B, A](f: A ⇒ A)(self: B)(implicit bp: Biplate[B, A]) = bp.transformBi(f)(self)
}
