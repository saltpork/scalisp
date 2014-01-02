#!/bin/bash
# -*- mode: scala -*-
exec env scala -deprecation "$0" "$@"
!#

//package scalisp

import util.parsing.combinator.RegexParsers
import collection.mutable.{Map => mMap}

//object `package` {
//}

// exceptions
case class Error(e : String) extends Exception(e)
case class ParseError(e : String) extends Exception(e)

// Scope
class Scope(var symtab : Map[String, Any] = Map[String, Any]()) {
  def +=(kv : (String, Any)) { symtab = symtab + kv }
  def copy = new Scope(symtab)
  def apply(s : String) = symtab(s)
  def isDefinedAt(s : String) = symtab isDefinedAt s
}
object Scope {
  def apply(kv : (String, Any)*) = new Scope(Map(kv : _*))
  def apply(prev : Scope, newBindings : (String, Any)*) = {
    val ret = prev.copy
    for (binding <- newBindings) ret += binding
    ret
  }
}

// Expressions
trait Expr { def eval(scope : Scope) : Any }
case class LiteralFloat(value : Float) extends Expr { def eval(scope : Scope) = value }
case class LiteralInt(value : Int) extends Expr { def eval(scope : Scope) = value }
case class LiteralString(value : String) extends Expr { def eval(scope : Scope) = value }
case class Symbol(name : String) extends Expr { 
  def eval(scope : Scope) = scope(name)
}
case class S(args : List[Expr]) extends Expr { 
  def eval(scope : Scope) = {
    args match {
      case Symbol("lambda") :: S(fargs) :: body =>
        val symargs = fargs.map { x => assert(x.isInstanceOf[Symbol], s"lambda argument $x is not a string!"); x.asInstanceOf[Symbol] }

        { (xs : List[Any]) =>
          val newScope = Scope(scope, newBindings = (symargs.map(_.name) zip xs) : _*)
          for (l <- body.dropRight(1)) l.eval(newScope)
          body.last.eval(newScope)
        }
      // MACRO
      // case Symbol("macro") :: Symbol(name) :: S(margs) :: body =>
      case Symbol("lambda") :: rest => throw Error(s"malformed lambda expression (lambda ${rest})")
      case (Symbol("set!") | Symbol("def")) :: Symbol(name) :: value :: Nil =>
        val res = value.eval(scope)
        scope += name -> res
        res
      case Symbol(fn) :: rest => // Function call
        if (scope isDefinedAt fn) {
          val evalArgs = args.tail.map(_.eval(scope))
          scope(fn).asInstanceOf[Function1[List[Any], Any]](evalArgs) //args.tail)
        }
        else throw Error(s"Function ${fn} not defined")
      case x @ _ => throw Error(s"$x [a literal value] cannot be used as a function")
    }
  }
}

// Reader
class Reader extends RegexParsers {
  override val whiteSpace = """([\t ]*(?<!\\);[^\n\r$]+|[\t ]+)""".r
  def float  = ("""[-+]?\d+\.\d+""".r | ("""[-+]?\d+""" <~ "f")) ^^ { x => LiteralFloat(x.toFloat) }
  def int    = """[-+]?\d+""".r ^^ { x => LiteralInt(x.toInt) }
  def string = """(?<!\\)".*?(?<!\\)"""".r ^^ { x => LiteralString(x.drop(1).dropRight(1)) }
  def symbol = """[^\d()\s][^\s()]*""".r ^^ { x => Symbol(x) }
  def sexpr  : Parser[Expr] = "(" ~> rep1(expr) <~ ")" ^^ { x => S(x) }
  def expr   = (float | int | string | symbol | sexpr)
  def apply(input: String) : Expr = parseAll(expr, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => throw ParseError(failure.msg)
    }
}

// Tests
implicit class X(a : Any) {
  def shouldBe(b : Any) = {
    val exprString = a.toString + " == " + b.toString
    var shortened  = exprString.substring(0, math.min(exprString.length, 97))
    if (shortened.length < exprString.length) shortened = shortened + "..."
    if (a == b) println(Console.GREEN + f"$shortened%-100s ${"[OK]"}%6s")
    else println(Console.RED + f"$shortened%-100s ${"[FAIL]"}%6s")
  }
}
val reader = new Reader
val scope = Scope()

// TODO these require static types
scope += "function" -> { (xs : List[Any]) => xs.foldLeft(0.0f)(_ + _.asInstanceOf[Float]) }
scope += "+" -> { (xs : List[Any]) => xs.foldLeft(0)(_ + _.asInstanceOf[Int]) }

reader("function") shouldBe Symbol("function")
reader(""""function"""") shouldBe LiteralString("function")
reader("1") shouldBe LiteralInt(1)
reader("2.2") shouldBe LiteralFloat(2.2f)
reader("(2)") shouldBe S(List(LiteralInt(2)))
reader("(2.2)") shouldBe S(List(LiteralFloat(2.2f)))
reader("(2.2 3)") shouldBe S(List(LiteralFloat(2.2f), LiteralInt(3)))
reader("(2.2 3 function)") shouldBe S(List(LiteralFloat(2.2f), LiteralInt(3), Symbol("function")))
reader("(function 2.2)") shouldBe S(List(Symbol("function"), LiteralFloat(2.2f)))
reader("(lambda (x) (+ x 1))") shouldBe S(List(Symbol("lambda"), S(List(Symbol("x"))), S(List(Symbol("+"), Symbol("x"), LiteralInt(1)))))
reader("(function 2.2 3.3 (function 1.0 2.0))").eval(scope) shouldBe 8.5f
reader("(def incr (lambda (x) (+ x 1)))").eval(scope).toString shouldBe "<function1>"
reader("(set! y 4)").eval(scope) shouldBe 4
reader("(def add-y (lambda (x) (+ x y)))").eval(scope).toString shouldBe "<function1>"
reader("(incr 1)").eval(scope) shouldBe 2
reader("(incr 4)").eval(scope) shouldBe 5
reader("(add-y 4)").eval(scope) shouldBe 8
reader("(add-y 1)").eval(scope) shouldBe 5

