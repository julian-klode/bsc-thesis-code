/*
 * hlist.scala - Implementation of HLists
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
 * An implementation of HLists and zippers.
 *
 * For now, stick to HLists and zippers only. It might make sense to combine
 * this with LIGD though, and use HLists to view constructors in RType
 * instances rather than tuples. It should be possible to do this, although
 * I have not taken a look at that yet.
 */
object HLists {
  import scala.language.higherKinds
  import scala.language.implicitConversions

  /** Heterogeneous list */
  sealed trait HList
  case class HCons[T, R <: HList](car: T, cdr: R) extends HList {
    def ::[G](g: G): G :: T :: R = HCons(g, this)
    override def toString() = car.toString() ++ " :: " ++ cdr.toString()
  }
  sealed trait HNil extends HList {
    def ::[G](g: G): G :: HNil = HCons(g, this)
    override def toString() = "HNil"
  }

  case object HNil extends HNil

  /** Define a nicer type alias */
  type ::[H, T <: HList] = HCons[H, T]

  sealed trait First[T <: HList] {
    type Out
    def apply(t: T): Out
  }

  implicit def firstCons[A, B <: HList] = new First[A :: B] {
    override type Out = A
    override def apply(t: A :: B) = t.car
  }

  sealed trait Last[T <: HList] {
    type Out
    def apply(t: T): Out
  }

  implicit def lastConsCut[A] = new Last[A :: HNil] {
    override type Out = A
    override def apply(t: A :: HNil) = t.car
  }

  implicit def lastCons[A, B <: HList](implicit lb: Last[B]) = new Last[A :: B] {
    override type Out = lb.Out
    override def apply(t: A :: B) = lb(t.cdr)
  }

  sealed trait Tail[T <: HList] {
    type Out <: HList
    def apply(t: T): Out
  }

  implicit def tailCons[A, B <: HList] = new Tail[A :: B] {
    override type Out = B
    override def apply(t: A :: B) = t.cdr
  }

  sealed trait Append[T <: HList, V] {
    type Out <: HList
    def apply(t: T, v: V): Out
  }

  implicit def appendHCons[A, B <: HList, V](implicit a: Append[B, V]) = new Append[A :: B, V] {
    override type Out = A :: a.Out
    override def apply(t: A :: B, v: V) = HCons(t.car, a(t.cdr, v))
  }

  implicit def appendHNil[V] = new Append[HNil, V] {
    override type Out = V :: HNil
    override def apply(t: HNil, v: V) = v :: HNil
  }

  sealed trait Init[T <: HList] {
    type Out <: HList
    def apply(t: T): Out
  }

  implicit def initConsCut[A]: Init[A :: HNil] = new Init[A :: HNil] {
    override type Out = HNil
    override def apply(t: A :: HNil) = HNil
  }

  implicit def initCons[A, B <: HList](implicit lb: Init[B]): Init[A :: B] = new Init[A :: B] {
    override type Out = A :: lb.Out
    override def apply(t: A :: B) = HCons(t.car, lb.apply(t.cdr))
  }

  sealed trait ToNestedTuple[T <: HList] {
    type Out
    def apply(t: T): Out
  }

  implicit def toNestedTupleNil = new ToNestedTuple[HNil] {
    type Out = Unit
    def apply(t: HNil) = ()
  }
  implicit def toNestedTuple[A, B <: HList](implicit nb: ToNestedTuple[B]) = new ToNestedTuple[A :: B] {
    type Out = (A, nb.Out)
    def apply(t: A :: B) = (t.car, nb(t.cdr))
  }

  sealed trait FromNestedTuple[T] {
    type Out <: HList
    def apply(t: T): Out
  }

  implicit def fromNestedTupleCutOff[A] = new FromNestedTuple[(A, Unit)] {
    type Out = A :: HNil
    def apply(t: (A, Unit)) = t._1 :: HNil
  }
  implicit def fromNestedTuplePair[A, B](implicit nb: FromNestedTuple[B]) = new FromNestedTuple[(A, B)] {
    type Out = A :: nb.Out
    def apply(t: (A, B)) = t._1 :: nb(t._2)
  }

  def fromNestedTuple[A](a: A)(implicit fromNestedTuple: FromNestedTuple[A]) = fromNestedTuple(a)

  /**
   * Methods for the
   */
  class HListMethods[C <: HList](c: C) {
    def ::[G](g: G): G :: C = HCons(g, c)
    def first(implicit first: First[C]) = first(c)
    def last(implicit last: Last[C]) = last(c)
    def tail(implicit tail: Tail[C]) = tail(c)
    def init(implicit init: Init[C]) = init(c)
    def toNestedTuple(implicit toNestedTuple: ToNestedTuple[C]) = toNestedTuple(c)
    def append[V](v: V)(implicit append: Append[C, V]) = append(c, v)
  }
  implicit def op[C <: HList](c: C): HListMethods[C] = new HListMethods(c)

  /**
   * Implementation of a zipper for HLists
   *
   * As long as your type has an RType (can be represented as an HList), you
   * can use it here.
   */
  case class Zipper[P <: HList, S <: HList, U](val pre: P, val suf: S, val up: Option[U]) {
    def get(implicit first: First[S]): first.Out = first(suf)
    def left(implicit tail: Tail[P], first: First[P]) = new Zipper(tail(pre), HCons(first(pre), suf), up)
    def right(implicit tail: Tail[S], first: First[S]) = Zipper(first(suf) :: pre, tail(suf), up)
    /* TODO: Want to apply directly, currently need to down .right.apply */
    def down(implicit first: First[S]) = new Object {
      def apply(implicit rep: RType[first.Out]) = Zipper(HNil, rep.toHList(first(suf)), Some(Zipper.this))
    }
  }

  def zipper[L <: HList](l: L) = Zipper(HNil, l, None)

  import HLists._

  trait RType[T] {
    type HListType <: HList

    def toHList: T ⇒ HListType
    def fromHList: HListType ⇒ T
  }

  /**
   * Example: Represent a tuple as an HList
   */
  case class RTuple[A, B]() extends RType[(A, B)] {
    override type HListType = A :: B :: HNil
    override def toHList = t ⇒ t._1 :: t._2 :: HNil
    override def fromHList = t ⇒ (t.car, t.cdr.car)
  }

  implicit def rTuple[A, B] = RTuple[A, B]

  def object2HList[T](t: T)(implicit rep: RType[T]): rep.HListType = rep.toHList(t)
  def zipper[T](t: T)(implicit rep: RType[T]) = Zipper[HNil, rep.HListType, Option[Nothing]](HNil, rep.toHList(t), None)

  /**
   * Dangerous LIGD world.
   *
   * This produces fully working Rep instances for an RType instance above. It
   * does not work implicitly, though; because of Scala limits on refering to
   * a dependent type that is in the same list of parameters.
   */
  object ligd {
    import LIGD._
    import scala.language.reflectiveCalls

    sealed trait ToNestedTuple2[T <: HList] {
      type Out
      val rep: Rep[Out]
      def apply(t: T): Out
    }

    implicit def toNestedTuple2Nil = new ToNestedTuple2[HNil] {
      type Out = Unit
      val rep = LIGD.rep[Unit]
      def apply(t: HNil) = ()
    }
    implicit def toNestedTuple2[A: Rep, B <: HList](implicit nb: ToNestedTuple2[B]) = new ToNestedTuple2[A :: B] {
      type Out = (A, nb.Out)
      val rep = LIGD.rep[(A, nb.Out)](RProd(LIGD.rep[A], nb.rep))
      def apply(t: A :: B) = (t.car, nb(t.cdr))
    }
    sealed trait FromNestedTuple2[T] {
      type Out <: HList
      def apply(t: T): Out
    }

    implicit def fromNestedTuple2CutOff[A: Rep] = new FromNestedTuple2[(A, Unit)] {
      type Out = A :: HNil
      def apply(t: (A, Unit)) = t._1 :: HNil
    }
    implicit def fromNestedTuple2Pair[A: Rep, B: Rep](implicit nb: FromNestedTuple2[B]) = new FromNestedTuple2[(A, B)] {
      type Out = A :: nb.Out
      def apply(t: (A, B)) = t._1 :: nb(t._2)
    }

    implicit val RHNil = new LIGD.RType(
      RUnit,
      EP((nil: HNil) ⇒ (), (unit: Unit) ⇒ HNil)
    )

    /* A Rep that converts HLists to nested pairs */
    implicit def RHList[A: Rep, B <: HList](implicit t: ToNestedTuple2[A :: B]) = new {
      def apply(implicit f: FromNestedTuple2[t.Out]) = LIGD.RType(
        t.rep,
        EP((x: f.Out) ⇒ t(x.asInstanceOf[A :: B]),
          (x: t.Out) ⇒ f(x))
      )
    }

    /* A Rep that converts custom types to HLists */
    def RepAnyWithList[T](implicit rt: HLists.RType[T]) = new {
      def apply(implicit rl: LIGD.RType[_, rt.HListType]): Rep[T] = new LIGD.RType(rl,
        EP(
          (t: T) ⇒ rt.toHList(t),
          (t: rt.HListType) ⇒ rt.fromHList(t)
        )
      )
    }

    val rl = RHList[Int, Int :: HNil].apply
    val ra = RepAnyWithList[(Int, Int)]
    val eq = LIGD.geq((1, 2), (1, 2))(ra.apply(rl))
  } /* ligd */
}