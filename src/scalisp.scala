package scalisp

import util.parsing.combinator.{RegexParsers, PackratParsers}
import collection.mutable.{Map => mMap, HashMap, ArrayBuffer}
import java.io.{Reader => jReader}
import tools.jline.console.ConsoleReader
import tools.jline.console.history.FileHistory
import language.dynamics
import annotation.tailrec

object `package` {
  def mapFromList(list : List[Any]) = Map(list.grouped(2).map { case List(a, b) => a -> b }.toList : _*)

  // ------------------------------------------------------------------------------------------------------------
  // Eval
  // ------------------------------------------------------------------------------------------------------------
  def countSplice(tree : Any, depth : Int = 1) : (Int, Any) = tree match {
      case (Symbol("splice") | Symbol("spliceseq")) :: x :: Nil => countSplice(x, depth = depth + 1)
      case rest @ _ => depth -> rest
    }
  def splice(expr : Any, scope : Scope, depth : Int = 1) : Any = expr match {
      case 'quasiquote :: x :: Nil => List('quasiquote, splice(x, scope, depth = depth + 1))
      case full @ (Symbol("splice") :: x :: Nil) => 
        val (count, tail) = countSplice(x)
        if (depth == count) eval(tail, scope)
        else full
      case full @ ('spliceseq :: x :: Nil) => 
        val (count, tail) = countSplice(x)
        if (depth == count) List('paste, eval(x, scope))
        else full
      case args : Map[_, _] => args map { case (k, v) => splice(k, scope, depth) -> splice(v, scope, depth) }
      case args : Iterable[Any] =>
        args.flatMap(ex => splice(ex, scope, depth) match { 
                       case List('paste, x : Seq[Any]) => x
                       case x @ _ => List(x)
                     })
      case ex @ _ => ex
    }
  def eval(expr : Any, scope : Scope) : Any = expr match {
      case name : Symbol => scope(name)
      case args : List[Any] =>
        args match {
          // Special forms
          case 'lambda :: (fargs : List[Any]) :: body =>
            val barr = body.toArray
            if (barr.length == 0) throw Error(s"malformed lambda with no body: (lambda $fargs $body)")
            val symargs  = fargs.map { x => assert(x.isInstanceOf[Symbol], s"lambda argument $x is not a string!"); x.asInstanceOf[Symbol] }

            { (xs : List[Any]) =>
              val newScope = Scope(scope, newBindings = (symargs zip xs) : _*)
              var idx = 0
              while(idx < (barr.length-1)) {
                eval(barr(idx), newScope)
                idx += 1
              }
              eval(barr(barr.length - 1), newScope)
            }
          case 'lambda :: rest => throw Error(s"malformed lambda expression (lambda ${rest})")
          case 'quote :: rest :: Nil => rest
          case 'quasiquote :: rest :: Nil => splice(rest, scope)
          case sp @ (('splice | 'spliceseq) :: x :: Nil) => sp
          case (Symbol("set!") | 'def) :: (name : Symbol) :: value :: Nil =>
            val res = eval(value, scope)
            scope(name) = res
            res
          case Symbol("macro") :: (name : Symbol) :: (margs : List[Any]) :: body =>
            val symargs = margs.map { x => assert(x.isInstanceOf[Symbol], s"macro argument $x is not a string!"); x.asInstanceOf[Symbol] }

            val mac = Macro({ (xs : List[Any]) =>
                              val newScope = Scope(scope, newBindings = (symargs zip xs) : _*)
                              var res : Any = 0
                              for (l <- body) res = eval(l, newScope)
                              res
                            })
            scope(name) = mac
            mac
          case 'if :: condExpr :: alt1 :: alt2 :: Nil =>
            val cond = eval(condExpr, scope).asInstanceOf[Boolean]
            if (cond) eval(alt1, scope)
            else eval(alt2, scope)
          case fn :: rest => // Function call
            eval(fn, scope) match {
              case Macro(mac) =>
                val expr = mac(rest)
                eval(expr, scope)
              case func : Function1[_, _] =>
                val evalArgs = rest.map(e => eval(e, scope))
                func.asInstanceOf[Function1[List[Any], Any]](evalArgs)
              case x @ _ => throw Error(s"$x [a literal value] cannot be used as a function")
            }
        }
      case map : Map[_, _] => map.map { case (k, v) => eval(k, scope) -> eval(v, scope) }
      case iter : Iterable[Any] => iter.map(v => eval(v, scope))
      case x @ _ => x
    }

  // ------------------------------------------------------------------------------------------------------------
  // Print
  // ------------------------------------------------------------------------------------------------------------
  def lispString(expr : Any) : String =
    expr match {
      case Symbol(s)          => s"'$s"
      case List(xs @ _*)      => s"(${xs.map(e => lispString(e)).mkString(" ")})"
      case Vector(xs @ _*)    => s"[${xs.map(e => lispString(e)).mkString(" ")}]"
      case xs : Set[_]        => s"#{${xs.map(e => lispString(e)).mkString(" ")}}"
      case xs : Map[_, _]     => s"{${xs.map{ case (k, v) => lispString(k) + " " + lispString(v) }.mkString(" ")}}"
      case c : Char           => s"#\\$c"
      case e @ _              => e.toString
    }
}

// ------------------------------------------------------------------------------------------------------------
// Exceptions and helpers
// ------------------------------------------------------------------------------------------------------------
case class Error(e : String) extends Exception(e)
case class ParseError(e : String) extends Exception(e)
case class ArgumentError(e : String) extends Exception(e)
case class NotImplementedError(e : String) extends Exception(e)
case class BindingNotFound(e : String) extends Exception(e)

// ------------------------------------------------------------------------------------------------------------
// Scope
// ------------------------------------------------------------------------------------------------------------
class Scope(var symtab : Map[Symbol, Any] = Map[Symbol, Any]()) {
  final def update(k : Symbol, v : Any) { symtab += k -> v }
  final def apply(k : Symbol) : Any = symtab(k)
  final def isDefinedAt(k : Symbol) = symtab isDefinedAt k
}
object Scope {
  def apply(kv : (Symbol, Any)*) = new Scope(Map(kv : _*))
  def apply(prev : Scope, newBindings : (Symbol, Any)*) = new Scope(prev.symtab ++ newBindings)
}

// ------------------------------------------------------------------------------------------------------------
// Functions/macros
// ------------------------------------------------------------------------------------------------------------
case class Macro(fn : List[Any] => Any)

class MultiMethod(methods : ArrayBuffer[PartialFunction[Product, Any]] = ArrayBuffer[PartialFunction[Product, Any]]()) extends Function1[List[Any], Any] {  
  case class Zero()
  val fail : PartialFunction[Product, Any] = { case args @ _ => throw ArgumentError(s"No method matches $args") }
  val fn = methods.reduce(_ orElse _) orElse fail 
  def defMethod(m : PartialFunction[Product, Any]) = methods += m
  def apply(args : List[Any]) = {
    val argTuple = args.length match {
        case 0 => Zero()
        case 1 => Tuple1(args(0))
        case 2 => args(0) -> args(1)
        case 3 => (args(0), args(1), args(2))
        case 4 => (args(0), args(1), args(2), args(3))
        case 5 => (args(0), args(1), args(2), args(3), args(4))
        case 6 => (args(0), args(1), args(2), args(3), args(4), args(5))
        case 7 => (args(0), args(1), args(2), args(3), args(4), args(5), args(6))
      }
    fn(argTuple)
  }
}
object MultiMethod {
  def apply(methods : PartialFunction[Product, Any]*) = new MultiMethod(ArrayBuffer(methods : _*))
}

// ------------------------------------------------------------------------------------------------------------
// Reader
// ------------------------------------------------------------------------------------------------------------
class Reader extends RegexParsers with PackratParsers{
  // TODO: 
  //  + missing support for string escape sequences
  //  + etc.
  val FloatWithDotMatcher  = """[-+]?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?[Ff]?""".r
  val FloatNoDotMatcher    = """[-+]?[0-9]*[0-9]+([eE][-+]?[0-9]+)?[Ff]""".r
  val readTable            = mMap[Char, Parser[Any]](
      '#'  -> ("{" ~> rep(expr) <~ "}" ^^ { _.toSet }),
      '{'  -> (rep(expr) <~ "}" ^^ { ex => mapFromList(ex) }),
      '['  -> (rep(expr) <~ "]" ^^ { _.toVector }),
      '\'' -> (expr ^^ { ex => List('quote, ex) }),
      '`'  -> (expr ^^ { ex => List('quasiquote, ex) }),
      '~'  -> (opt("@") ~ expr ^^ { 
                 case Some("@") ~ ex => List('spliceseq, ex)
                 case None ~ ex      => List('splice, ex) })
    )

  override val whiteSpace = """([\s\n\r]*(?<!\\);[^\n\r$]+[\n\r\s$]*|[\s\n\r]+)""".r // TODO: doesn't handle strings containing ';'

  def double   = """[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?[dD]""".r ^^ { x => x.toDouble }
  def float    = (FloatWithDotMatcher | FloatNoDotMatcher ) ^^ { x => x.toFloat }
  def int      = """[-+]?\d+""".r ^^ { x => x.toInt }
  def uchar    = """\\(u[\da-fA-F]{4})""".r ^^ { x => Integer.parseInt(x.drop(2), 16).toChar }
  def achar    = """\\[\da-fA-F]{2}""".r ^^ { x => Integer.parseInt(x.drop(1), 16).toChar }
  def char     = """\\.""".r ^^ { x => augmentString(x)(1) }
  def string   = """(?<!\\)".*?(?<!\\)"""".r ^^ { x => x.drop(1).dropRight(1) }
  def bools    = """(?iu)(true|false)""".r ^^ { x => x.toLowerCase match { case "true" => true; case "false" => false } }
  def d_hash   = "#" ~> readTable('#')
  def d_quote  = "'" ~> readTable('\'')
  def d_tilde  = "~" ~> readTable('~')
  def d_quasi  = "`" ~> readTable('`')
  def d_at     = "@" ~> readTable('@')
  def d_ocurly = "{" ~> readTable('{')
  def d_obrac  = "[" ~> readTable('[')
  def symbol   = """[^\d(){}#'`,@~;~\[\]^\s][^\s()#'`,@~;^{}~\[\]]*""".r ^^ { x => Symbol(x) }
  def sexpr  : Parser[Any] = "(" ~> rep(expr) <~ ")"
  def expr   : Parser[Any] = (double | float | int | uchar | achar | char | string | bools | symbol | sexpr | 
                              d_hash | d_quote | d_ocurly | d_obrac | d_quasi | d_tilde)

  def apply(input : String) : Any = parseAll(expr, input) match {
      case Success(result, _) => result
      case failure @ _ => throw ParseError(failure.toString)
    }
  def apply(input : jReader) : Any = parseAll(expr, input) match {
      case Success(result, _) => result
      case failure @ _ => throw ParseError(failure.toString)
    }
}

// ------------------------------------------------------------------------------------------------------------
// REPL
// ------------------------------------------------------------------------------------------------------------
object REPL {
  def prompt = Console.BLUE + "scalisp> " + Console.YELLOW
  val console = {
    val consoleReader = new ConsoleReader(System.in, System.out, null, null)
    consoleReader setHistory (new FileHistory(new java.io.File(".scalisp-history")))
    consoleReader setHistoryEnabled true
    consoleReader setPrompt prompt
    consoleReader
  }
  def main(args : Array[String]) {
    val reader = new Reader
    val scope  = Scope(lib.DefaultEnvironment : _*)
    var x = 0

    while(true) {
      console.readLine() match {
        case l : String =>
          val start = System.nanoTime
          val res    = lispString(eval(reader(l), scope))
          val dur   = System.nanoTime - start
          println(Console.CYAN + res)
          println(Console.RED  + f"result took ${dur.toDouble / 1e9}%7.2f seconds")
        case _ => 
          println(Console.RED + "exiting...")
          System.exit(0)
      }
    }

  }
}

