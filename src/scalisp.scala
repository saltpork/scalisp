package scalisp

import util.parsing.combinator.RegexParsers
import java.io.{Reader => jReader}
import tools.jline.console.ConsoleReader
import tools.jline.console.history.FileHistory

object `package` {
  // ------------------------------------------------------------------------------------------------------------
  // Eval
  // ------------------------------------------------------------------------------------------------------------
  def eval(expr : Any, scope : Scope) : Any = expr match {
      case Symbol(name) => scope(name)
      case args : List[Any] =>
        args match {
          // Special forms
          case Symbol("lambda") :: (fargs : List[Any]) :: body =>
            val symargs = fargs.map { x => assert(x.isInstanceOf[Symbol], s"lambda argument $x is not a string!"); x.asInstanceOf[Symbol] }

            { (xs : List[Any]) =>
              val newScope = Scope(scope, newBindings = (symargs.map(_.name) zip xs) : _*)
              for (l <- body.dropRight(1)) eval(l, newScope)
              eval(body.last, newScope)
            }
          case Symbol("lambda") :: rest => throw Error(s"malformed lambda expression (lambda ${rest})")
          case Symbol("quote") :: rest :: Nil => rest
          case (Symbol("set!") | Symbol("def")) :: Symbol(name) :: value :: Nil =>
            val res = eval(value, scope)
            scope += name -> res
            res
          case Symbol("macro") :: Symbol(name) :: (margs : List[Any]) :: body =>
            val symargs = margs.map { x => assert(x.isInstanceOf[Symbol], s"macro argument $x is not a string!"); x.asInstanceOf[Symbol] }

            val mac = Macro({ (xs : List[Any]) =>
                              val newScope = Scope(scope, newBindings = (symargs.map(_.name) zip xs) : _*)
                              for (l <- body.dropRight(1)) eval(l, newScope)
                              val x = eval(body.last, newScope)
                              x
                            })
            scope += name -> mac
            mac
          case Symbol("if") :: condExpr :: then :: alt :: Nil =>
            val cond = eval(condExpr, scope).asInstanceOf[Boolean]
            if (cond) eval(then, scope)
            else eval(alt, scope)
          case fn :: rest => // Function call
            eval(fn, scope) match {
              case Macro(mac) =>
                val expr = mac(rest)
                eval(expr, scope)
              case func : Function1[_, _] =>
                val evalArgs = rest.map(e => eval(e, scope))
                func.asInstanceOf[Function1[List[Any], Any]](evalArgs) //args.tail)
              case x @ _ => throw Error(s"$x [a literal value] cannot be used as a function")
            }
            //else throw Error(s"Function ${fn} not defined")
          
        }
      case x @ _ => x
    }

  // ------------------------------------------------------------------------------------------------------------
  // Print
  // ------------------------------------------------------------------------------------------------------------
  def lispString(expr : Any) : String =
    expr match {
      case Symbol(s)     => s"'$s"
      case List(xs @ _*) => s"(${xs.map(e => lispString(e)).mkString(" ")})"
      case c : Char      => s"#\\$c"
      case e @ _         => e.toString
    }

  // ------------------------------------------------------------------------------------------------------------
  // Basic function environment
  // ------------------------------------------------------------------------------------------------------------
  val DefaultEnvironment = List(
      // TODO: These are int only stubs, should promote then evaluate
      "+"      -> { (xs : List[Any]) => xs.foldLeft(0)(_ + _.asInstanceOf[Int]) },
      "-"      -> { (xs : List[Any]) => xs.tail.foldLeft(xs.head.asInstanceOf[Int])(_ - _.asInstanceOf[Int]) },

      "cons"   -> { (xs : List[Any]) => xs match { 
                              case a :: (b : List[Any]) :: Nil => a :: b
                              case e @ _ => throw Error(s"cons called with illegal args: $e") }
        },
      "list"   -> { (xs : List[Any]) => List(xs : _*) },
      "rest"   -> { (xs : List[Any]) => xs match { 
                              case (a : List[Any]) :: Nil => a.tail
                              case e @ _ => println(s"ERROR: $e"); throw Error("rest") }
        },
      "print"  -> { (xs : List[Any]) => println(Console.YELLOW + xs.foldLeft("")(_ + _)) }
    )
}

case class Macro(fn : List[Any] => Any)

// ------------------------------------------------------------------------------------------------------------
// exceptions
// ------------------------------------------------------------------------------------------------------------
case class Error(e : String) extends Exception(e)
case class ParseError(e : String) extends Exception(e)

// ------------------------------------------------------------------------------------------------------------
// Scope
// ------------------------------------------------------------------------------------------------------------
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

// ------------------------------------------------------------------------------------------------------------
// Reader
// ------------------------------------------------------------------------------------------------------------
class Reader extends RegexParsers {
  // TODO: 
  //  + character literals
  //  + macro dispatch
  //  + quote/quasiquote/unquote
  //  + missing support for string escape sequences
  //  + etc.
  val FloatWithDotMatcher  = """[-+]?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?[Ff]?""".r
  val FloatNoDotMatcher    = """[-+]?[0-9]*[0-9]+([eE][-+]?[0-9]+)?[Ff]""".r

  override val whiteSpace = """([\s\n\r]*(?<!\\);[^\n\r$]+[\n\r\s$]*|[\s\n\r]+)""".r // TODO: doesn't handle strings containing ';'

  def double   = """[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?[dD]""".r ^^ { x => x.toDouble }
  def float    = (FloatWithDotMatcher | FloatNoDotMatcher ) ^^ { x => x.toFloat }
  def int      = """[-+]?\d+""".r ^^ { x => x.toInt }
  def uchar    = """\\(u[\da-fA-F]{4})""".r ^^ { x => Integer.parseInt(x.drop(3), 16).toChar }
  def achar    = """\\[\da-fA-F]{2}""".r ^^ { x => Integer.parseInt(x.drop(2), 16).toChar }
  def char     = """\\.""".r ^^ { x => augmentString(x)(2) }
  def string   = """(?<!\\)".*?(?<!\\)"""".r ^^ { x => x.drop(1).dropRight(1) }
  def bools    = """(?iu)(true|false)""".r ^^ { x => x.toLowerCase match { case "true" => true; case "false" => false } }
  //def dispatch = """[#'`,@;^]""".r ^^ { x => readTable(x) }
  def symbol   = """[^\d()\s][^\s()]*""".r ^^ { x => Symbol(x) }
  def sexpr  : Parser[Any] = "(" ~> rep1(expr) <~ ")"
  def expr     = (double | float | int | uchar | achar | char | string | bools | symbol | sexpr)

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
    val consoleReader = new ConsoleReader(System.in, new java.io.OutputStreamWriter(System.out))
    consoleReader setHistory (new FileHistory(new java.io.File(".scalisp-history")))
    consoleReader setHistoryEnabled true
    consoleReader setPrompt prompt
    consoleReader
  }
  def main(args : Array[String]) {
    val reader = new Reader
    val scope  = Scope(DefaultEnvironment : _*)

    while(true) {
      console.readLine() match {
        case l : String =>
          println(Console.CYAN + lispString(eval(reader(l), scope)))
        case _ => 
          println(Console.RED + "exiting...")
          System.exit(0)
      }
    }

  }
}

// ------------------------------------------------------------------------------------------------------------
// Tests
// ------------------------------------------------------------------------------------------------------------
object Tests {
  var total  = 0
  var errors = 0
  // Tests
  implicit class X(a : Any) {
    def shouldBe(b : Any) = {
      val exprString = a.toString + " == " + b.toString
      var shortened  = exprString.substring(0, math.min(exprString.length, 97))
      if (shortened.length < exprString.length) shortened = shortened + "..."
      total += 1
      if (a == b) println(Console.GREEN + f"$shortened%-100s ${"[OK]"}%6s")
      else { errors += 1; println(Console.RED + f"$shortened%-100s ${"[FAIL]"}%6s") }
    }
  }
  def header(s : String) = {
    println()
    println(Console.YELLOW + s)
    println(Console.YELLOW + ("-" * s.length))
  }
  def summary = {
    val color = if (errors > 0) Console.MAGENTA else Console.CYAN
    println()
    val res     = f"Summary: $errors%d tests failed out of $total%d [accuracy: ${(total - errors) * 100.0 / total}%.2f%%]"
    val colored = Console.WHITE + "Summary: " + color + f"$errors%d tests failed out of $total%d [accuracy: ${(total - errors) * 100.0 / total}%.2f%%]"
    println(Console.WHITE + ("=" * res.length))
    println(Console.WHITE + colored)
    println(Console.WHITE + ("=" * res.length))
    println(Console.RESET)
  }

  def main(args : Array[String]) {
    val reader = new Reader
    val scope  = Scope(DefaultEnvironment : _*)

    scope += "function" -> { (xs : List[Any]) => xs.foldLeft(0.0f)(_ + _.asInstanceOf[Float]) }

    header("Reading simple expressions")
    reader("function") shouldBe Symbol("function")
    reader(""""function"""") shouldBe "function"
    reader("1") shouldBe 1
    reader("2.2") shouldBe 2.2f
    reader("2f") shouldBe 2.0f
    reader("3.3d") shouldBe 3.3
    reader("3d") shouldBe 3.0
    reader("1 ; this is a test") shouldBe 1
    reader("""|; this is a test
              |  3.3f""".stripMargin) shouldBe 3.3f
    reader("true") shouldBe true
    reader("false") shouldBe false
    reader("\\u236a") shouldBe '\u236a'
    reader("\\62") shouldBe 'b'
    reader("\\X") shouldBe 'X'

    header("Reading s-expressions")
    reader("(2)") shouldBe List(2)
    reader("(2.2)") shouldBe List(2.2f)
    reader("(2.2 3)") shouldBe List(2.2f, 3)
    reader("(2.2 3 function)") shouldBe List(2.2f, 3, Symbol("function"))
    reader("(function 2.2)") shouldBe List(Symbol("function"), 2.2f)
    reader("(lambda (x) (+ x 1))") shouldBe List(Symbol("lambda"), List(Symbol("x")), List(Symbol("+"), Symbol("x"), 1))

    header("Read and evaluate")
    eval(reader("(function 2.2 3.3 (function 1.0 2.0))"), scope) shouldBe 8.5f
    eval(reader("((lambda (x) (+ x 1)) 1)"), scope) shouldBe 2

    header("Special forms")
    eval(reader("(if true 1 2)"), scope) shouldBe 1
    eval(reader("(if false 1 2)"), scope) shouldBe 2
    eval(reader("(if (if true false true) 1 2)"), scope) shouldBe 2
    eval(reader("(if (if false false true) 1 2)"), scope) shouldBe 1
    eval(reader("(def incr (lambda (x) (+ x 1)))"), scope).toString shouldBe "<function1>"
    eval(reader("(incr 1)"), scope) shouldBe 2
    eval(reader("(incr 4)"), scope) shouldBe 5
    eval(reader("(set! y 4)"), scope) shouldBe 4
    eval(reader("(def add-y (lambda (x) (+ x y)))"), scope).toString shouldBe "<function1>"
    eval(reader("(add-y 4)"), scope) shouldBe 8
    eval(reader("(add-y 1)"), scope) shouldBe 5

    header("Macros and quoting")
    eval(reader("(quote (1 2 3))"), scope) shouldBe List(1, 2, 3)
    eval(reader("(quote (+ 1 4))"), scope) shouldBe List('+, 1, 4)
    eval(reader("(macro test (x) (cons (quote -) (rest x)))"), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(test (+ 1 4))"), scope) shouldBe -3
    eval(reader("(macro m (x) (print \"-- Evaluating: \" x) x)"), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(m (+ 1 3))"), scope) shouldBe 4
    eval(reader("(macro define (name args body) (list (quote def) name (list (quote lambda) args body)))"), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(define addxy (x y) (+ x y))"), scope).toString shouldBe "<function1>"
    eval(reader("(addxy 1 3)"), scope) shouldBe 4

    summary
  }
}
