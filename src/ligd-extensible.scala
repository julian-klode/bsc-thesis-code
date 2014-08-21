/*
 * lidg-extensible.scala - Extensible generic functions in LIGD
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

import LIGD._

/**
 * Examples illustrating how to write functions in an extensible style.
 *
 * This has not appeared in any of the referenced literature, and a quick
 * search did not show any other literature.
 *
 * This approach demonstrated here is class-based. For Haskell, a similar
 * approach can be used:
 *   1. Convert geq :: Rep[a] -> a -> a to
 *              geq_ :: Rep[a] -> a -> a -> (forall a. Rep[a] -> a -> a)
 *      and change self-recursive calls to use the third parameter instead
 *   2. Add a geq :: Rep[a] -> a -> a where
 *      geq rep a b = geq_ rep a b geq
 *
 * This approach would work in Scala as well, but seems more complex at the
 * moment, due to a lack of good existential types. Implementing it with the
 * Returning-trait pattern would be more verbose then the direct approach
 * outlined below.
 */

object LIGDExtensible {

  /**
   * Convert geq to a class GEq with an apply method.
   *
   * This is similar to the conversion we have done for Returning, the only
   * difference is that GEq takes two arguments. So, it would implement some
   * sort of Returning2 trait, if we had one.
   */
  class geq {
    def apply[A: Rep](a: A, b: A): Boolean = (rep[A], a, b) match {
      case (RUnit, (), ())                      ⇒ true
      case (RBoolean, a, b)                     ⇒ a == b
      case (RInt, a, b)                         ⇒ a == b
      case (RFloat, a, b)                       ⇒ a == b
      case (RChar, a, b)                        ⇒ a == b
      case (RString, a, b)                      ⇒ a == b
      case (RSum(ra, rb), Left(a1), Left(a2))   ⇒ apply(a1, a2)(ra)
      case (RSum(ra, rb), Right(b1), Right(b2)) ⇒ apply(b1, b2)(rb)
      case (RSum(_, _), _, _)                   ⇒ false
      case (RProd(ra, rb), (a1, b1), (a2, b2)) ⇒
        apply(a1, a2)(ra) && apply(b1, b2)(rb)
      case (r: RType[_, A], t1, t2) ⇒ apply(r.b.from(t1), r.b.from(t2))(r.a)
      case _                        ⇒ false

    }
  }

  /**
   * Extensibility works by overriding apply
   *
   * If we do not know the type, defer to the super implementation.
   *
   */
  class geqlist extends geq {
    override def apply[A: Rep](a: A, b: A): Boolean = (rep[A], a, b) match {
      case (RList(ra), xs, ys) ⇒ xs.length == ys.length && (
        xs.isEmpty ||
        xs.zip(ys).map({ case (x, y) ⇒ apply(x, y)(ra) }).min)
      case (r, a, b) ⇒ super.apply(a, b)
    }
  }

  /* Create objects that can be applied (like functions) */
  object geq extends geq
  object geqlist extends geqlist
}
