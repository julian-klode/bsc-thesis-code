/*
 * Based on:
 *  Comparing Libraries for Generic Programming in Haskell
 *  Technical Report UU-CS-2008-010
 * by:
 *  Alexey Rodriguez, Johan Jeuring, Patrik Jansson, Alex Gerdes,
 *  Oleg Kiselyov, Bruno C. d. S. Oliveira.
 */
import org.scalatest._

object LIGD {

  def geq[A](rep: Rep[A], a: A, b: A): Boolean = (rep, a, b) match {
    case (RUnit, Unit(), Unit())          ⇒ true
    case (RSum(ra, rb), Inl(a1), Inl(a2)) ⇒ geq(ra, a1, a2)
    case (RSum(ra, rb), Inr(b1), Inr(b2)) ⇒ geq(rb, b1, b2)
    case (RSum(ra, rb), _, _)             ⇒ false
    case (RProd(ra, rb), Prod(a1, b1), Prod(a2, b2)) ⇒
      geq(ra, a1, a2) && geq(rb, b1, b2)
    // FIXME: The [Any] is wrong, but makes it compile.
    case (RType(r: Rep[Any], ep: EP[A, Any]), t1, t2) ⇒ geq[Any](r, ep.from(t1), ep.from(t2))
    case _ ⇒ false

  }

  // Units, sums and products
  case class Unit // FIXME: Should be object, but fails below
  sealed abstract class Sum[A, B]
  sealed case class Inl[A, B](val a: A) extends Sum[A, B]
  sealed case class Inr[A, B](val b: B) extends Sum[A, B]
  sealed case class Prod[A, B](val a: A, val b: B)

  // Representation
  sealed abstract class Rep[+T]
  case object RUnit extends Rep[Unit]
  sealed case class RSum[A, B](val a: Rep[A], val b: Rep[B]) extends Rep[Sum[A, B]]
  sealed case class RProd[A, B](val a: Rep[A], val b: Rep[B]) extends Rep[Prod[A, B]]
  sealed case class RType[C, B](val c: Rep[C], val ep: EP[B, C]) extends Rep[B]

  // Isomorphism
  sealed case class EP[B, C](val from: B ⇒ C, val to: C ⇒ B)

  def fromList[A](list: List[A]): Sum[Unit, Prod[A, List[A]]] = list match {
    case Nil       ⇒ Inl(Unit())
    case (a :: as) ⇒ Inr(Prod(a, as))
  }

  def toList[A](list: Sum[Unit, Prod[A, List[A]]]): List[A] = list match {
    // FIXME: If Unit is a case object, this will fail
    case Inl(Unit())      ⇒ List.empty
    case Inr(Prod(a, as)) ⇒ a :: as
  }

  // TODO: This requires a type annotation for the EP, why?
  def rList[A](ra: Rep[A]): Rep[List[A]] = RType(RSum(RUnit, RProd(ra, rList(ra))), EP(fromList, toList): EP[List[A], Sum[Unit, Prod[A, List[A]]]])
}

class LIGDTests extends FlatSpec {
  import LIGD._
  "geq" should "work" in {
    assert(geq(RUnit, Unit(), Unit()))
  }
  "geq" should "support lists" in {
    assert(geq(rList(RUnit), List(Unit()), List(Unit())))
  }
}
