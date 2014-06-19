object LIGD {

    def geq[A](rep: Rep[A], a: A, b: A): Boolean = (rep, a, b) match {
        case (RUnit, Unit, Unit) => true
        case (RSum(ra, rb), Inl(a1), Inl(a2)) => geq(ra, a1, a2)
        case (RSum(ra, rb), Inr(b1), Inr(b2)) => geq(rb, b1, b2)
        case (RSum(ra, rb), _, _) => false
        case (RProd(ra, rb), Prod(a1, b1), Prod(a2, b2)) =>
            geq(ra, a1, a2) && geq(rb, b1, b2)
        case (RType(r, ep), t1, t2) => geq(r, ep.from(t1), ep.from(t2))
        case _ => false

    }

    // Units, sums and products
    case object Unit
    sealed abstract class Sum[A, B]
    sealed case class Inl[A, B](val a: A) extends Sum[A, B]
    sealed case class Inr[A, B](val b: B) extends Sum[A, B]
    sealed case class Prod[A, B](val a: A, val b: B)

    // Representation
    sealed abstract class Rep[+T]
    case object RUnit extends Rep[Unit]
    sealed case class RSum[A,B](val a: Rep[A], val b: Rep[B]) extends Rep[Sum[A,B]]
    sealed case class RProd[A,B](val a: Rep[A], val b: Rep[B]) extends Rep[Prod[A,B]]
    sealed case class RType[C, B](val c: Rep[C], val ep: EP[B,C])  extends Rep[B]
    

    // Isomorphism
    sealed case class EP[B, C](val from: B => C, val to: C => B)
    
}
