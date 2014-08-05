import org.scalatest._

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

  "biplate" should "work" in {

    implicit object BiplateListExpr extends Biplate[List[Expr], Expr] {
      override def biplate(self: List[Expr]) = (self, x ⇒ x)
    }

    def variables[B](self: B)(implicit bp: Biplate[B, Expr]): List[String] = {
      universeBi(self).map({ case Var(x) ⇒ x; case _ ⇒ null }).filter(_ != null)
    }

    val testList: List[Expr] = List(Add(Var("x"), Val(5)), Var("y"))

    assert(variables(testList) == List("x", "y"))
  }

}
