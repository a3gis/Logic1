/*
 * Fun with types!
 */

object Hello extends App {
	val s = Neg(Neg(Atom("X")))
	
	System.out.println(MT(Impl(Atom("A"), Atom("B")), Neg(Atom("B"))))
}

trait Expr { }

case class Atom(name: String) extends Expr
case class Neg[+T <: Expr](e: T) extends Expr
case class Impl[+T <: Expr, +U <: Expr](e1: T, e2: U) extends Expr

trait Rule { }

object DN1 extends Rule {
	def apply[T <: Expr](e: Neg[Neg[T]]): T = e match {
		case Neg(Neg(x)) => x
	}
}

object DN2 extends Rule {
	def apply[T <: Expr](e: T): Neg[Neg[T]] = Neg(Neg(e))
}

object MP extends Rule {
	def apply[T <: Expr, U <: Expr](e1: Impl[T, U], e2: T): U = e1 match {
		case Impl(ant, cons) if ant == e2 => cons
		case _ => throw new Error("Cannot apply modus ponens to expressions '" + e1 + "' and '" + e2 + "'")
	}
}

object MT extends Rule {
	def apply[T <: Expr, U <: Expr](e1: Impl[T, U], e2: Neg[U]): Neg[T] = e1 match {
		case Impl(ant, cons) if Neg(cons) == e2 => Neg(ant)
		case _ => throw new Error("Cannot apply modus tolens to expressions '" + e1 + "' and '" + e2 + "'")
	}
}