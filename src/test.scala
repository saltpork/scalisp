package scalisp
import collection.mutable.OpenHashMap

// ------------------------------------------------------------------------------------------------------------
// Tests
// ------------------------------------------------------------------------------------------------------------
object Tests {
  var total  = 0
  var errors = 0
  // Tests
  implicit class X(a : => Any) {
    def shouldBe(b : Any) = {
      total += 1
      try { 
        val ares = a
        val exprString = ares.toString + " == " + b.toString
        var shortened  = exprString.substring(0, math.min(exprString.length, 97))
        if (shortened.length < exprString.length) shortened = shortened + "..."

        if (ares == b) println(Console.GREEN + f"$shortened%-100s ${"[OK]"}%6s")
        else { errors += 1; println(Console.RED + f"$shortened%-100s ${"[FAIL]"}%6s") }
      }
      catch { case e : Throwable => 
        errors += 1 
        val s = s"$a threw an exception (message: $e)"
        println(Console.RED + f"$s%100s ${"[FAIL]"}%6s") 
      }
    }
    def shouldThrow(b : Exception) = {
      total += 1
      try {  
        val exprString = a.toString
        var shortened  = exprString.substring(0, math.min(exprString.length, 87))
        if (shortened.length < exprString.length) shortened = shortened + "..."
        errors += 1
        println(Console.RED + f"$shortened%-90s ${"[DIDN'T THROW]"}%16s")
      }
      catch { case e : Throwable => 
        if (e == b) println(Console.GREEN + f"${e.toString}%-90s ${"[OK]"}%16s")
        else { 
          errors += 1
          println(Console.RED + f"${"got " + e.toString + ", expected: " + b.toString}%-90s ${"[WRONG ERROR]"}%16s")
        }
      }
    }
  }
  def time(a : => Any) = {
    val start = System.nanoTime
    val res   = a
    val dur   = System.nanoTime - start
    dur.toDouble -> res
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

  def timeN(name : String, n : Int)(a : => Any) = {
    a // warm up
    val (dur, res) = time { a }
    println(Console.WHITE + f"TIME: $name%50s took ${dur / n.toDouble}%10.2f nanoseconds per iteration (total time: ${dur / 1e9}%7.3f seconds)")
    res
  }

  def main(args : Array[String]) {
    val reader = new Reader
    val scope  = Scope(lib.DefaultEnvironment : _*)

    scope('function) = Fn({ (xs : List[Any]) => xs.foldLeft(0.0f)(_ + _.asInstanceOf[Float]) })

    header("Reading simple expressions")
    reader("function", scope.symbolCache) shouldBe Symbol("function")
    reader(""""function"""", scope.symbolCache) shouldBe "function"
    reader("1", scope.symbolCache) shouldBe 1
    reader("2.2", scope.symbolCache) shouldBe 2.2f
    reader("2f", scope.symbolCache) shouldBe 2.0f
    reader("3.3d", scope.symbolCache) shouldBe 3.3
    reader("3d", scope.symbolCache) shouldBe 3.0
    reader("1 ; this is a test", scope.symbolCache) shouldBe 1
    reader("""|; this is a test
              |  3.3f""".stripMargin, scope.symbolCache) shouldBe 3.3f
    reader("true", scope.symbolCache) shouldBe true
    reader("false", scope.symbolCache) shouldBe false
    reader("\\u236a", scope.symbolCache) shouldBe '\u236a'
    reader("\\62", scope.symbolCache) shouldBe 'b'
    reader("\\X", scope.symbolCache) shouldBe 'X'

    header("Reading s-expressions")
    reader("(2)", scope.symbolCache) shouldBe List(2)
    reader("(2.2)", scope.symbolCache) shouldBe List(2.2f)
    reader("(2.2 3)", scope.symbolCache) shouldBe List(2.2f, 3)
    reader("(2.2 3 function)", scope.symbolCache) shouldBe List(2.2f, 3, Symbol("function"))
    reader("(function 2.2)", scope.symbolCache) shouldBe List(Symbol("function"), 2.2f)
    reader("(lambda (x) (+ x 1))", scope.symbolCache) shouldBe List(Symbol("lambda"), List(Symbol("x")), List(Symbol("+"), Symbol("x"), 1))

    header("Reading with reader macros")
    reader("#{a b c}", scope.symbolCache) shouldBe Set('a, 'b, 'c)
    reader("'(a b c)", scope.symbolCache) shouldBe List('quote, List('a, 'b, 'c))
    reader("{a b c d}", scope.symbolCache) shouldBe Map('a -> 'b, 'c -> 'd)
    reader("[a b c d]", scope.symbolCache) shouldBe Vector('a, 'b, 'c, 'd)
    reader("[a #{ b b } (c c) [d d]]", scope.symbolCache) shouldBe Vector('a, Set('b), List('c, 'c), Vector('d, 'd))
    reader("#{{a a b b} [b b] #{c d}}", scope.symbolCache) shouldBe Set(Map('a -> 'a, 'b -> 'b), Vector('b, 'b), Set('c, 'd))
    reader("`(test ~x ~@y)", scope.symbolCache) shouldBe List('quasiquote, List('test, List('splice, 'x), List('spliceseq, 'y)))

    header("Special forms")
    eval(reader("(if true 1 2)", scope.symbolCache), scope) shouldBe 1
    eval(reader("(if false 1 2)", scope.symbolCache), scope) shouldBe 2
    eval(reader("(if (if true false true) 1 2)", scope.symbolCache), scope) shouldBe 2
    eval(reader("(if (if false false true) 1 2)", scope.symbolCache), scope) shouldBe 1
    eval(reader("(def incr (lambda (x) (+ x 1)))", scope.symbolCache), scope)
    eval(reader("(incr 1)", scope.symbolCache), scope) shouldBe 2
    eval(reader("(incr 4)", scope.symbolCache), scope) shouldBe 5
    eval(reader("(set! y 4)", scope.symbolCache), scope) shouldBe 4
    eval(reader("(def add-y (lambda (x) (+ x y)))", scope.symbolCache), scope)
    eval(reader("(add-y 4)", scope.symbolCache), scope) shouldBe 8
    eval(reader("(add-y 1)", scope.symbolCache), scope) shouldBe 5

    header("Read and evaluate")
    eval(reader("(function 2.2 3.3 (function 1.0 2.0))", scope.symbolCache), scope) shouldBe 8.5f
    eval(reader("((lambda (x) (+ x 1)) 1)", scope.symbolCache), scope) shouldBe 2
    eval(reader("`(test ~y ~y)", scope.symbolCache), scope) shouldBe List('test, 4, 4)
    eval(reader("(set! z '(3 2 1))", scope.symbolCache), scope) shouldBe List(3, 2, 1)
    eval(reader("{ y y }", scope.symbolCache), scope) shouldBe Map(4 -> 4)
    eval(reader("{ y y 1 z }", scope.symbolCache), scope) shouldBe Map(4 -> 4, 1 -> List(3, 2, 1))
    eval(reader("[ y y { 1 z } ]", scope.symbolCache), scope) shouldBe Vector(4, 4, Map(1 -> List(3, 2, 1)))
    eval(reader("#{(incr 10) y}", scope.symbolCache), scope) shouldBe Set(11, 4)
    eval(reader("(first '(1 2 3))", scope.symbolCache), scope) shouldBe 1
    eval(reader("(rest '(1 2 3))", scope.symbolCache), scope) shouldBe List(2, 3)
    eval(reader("(first [1 2 3])", scope.symbolCache), scope) shouldBe 1
    eval(reader("(rest [1 2 3])", scope.symbolCache), scope) shouldBe Vector(2, 3)

    header("Macros and quoting")
    eval(reader("(quote (1 2 3))", scope.symbolCache), scope) shouldBe List(1, 2, 3)
    eval(reader("(quote (+ 1 4))", scope.symbolCache), scope) shouldBe List('+, 1, 4)
    eval(reader("(macro test (x) (cons (quote -) (rest x)))", scope.symbolCache), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(test (+ 1 4))", scope.symbolCache), scope) shouldBe -3
    eval(reader("(macro m (x) (print \"-- Evaluating: \" x) x)", scope.symbolCache), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(m (+ 1 3))", scope.symbolCache), scope) shouldBe 4
    eval(reader("(macro define (name args body) (list (quote def) name (list (quote lambda) args body)))", scope.symbolCache), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(define addxy (x y) (+ x y))", scope.symbolCache), scope)
    eval(reader("(addxy 1 3)", scope.symbolCache), scope) shouldBe 4
    eval(reader("(macro def2 (name args body) `(def ~name (lambda ~args ~body)))", scope.symbolCache), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(def2 addxy2 (x y) (+ x y))", scope.symbolCache), scope)
    eval(reader("(addxy2 1 3)", scope.symbolCache), scope) shouldBe 4
    eval(reader("`(test ~y ~@z)", scope.symbolCache), scope) shouldBe List('test, 4, 3, 2, 1)
    eval(reader("`{ ~y ~y }", scope.symbolCache), scope) shouldBe Map(4 -> 4)
    eval(reader("((lambda (y) `(print `(+ ~~y ~@z))) 7)", scope.symbolCache), scope) shouldBe List('print, List('quasiquote, List('+, 7, List('spliceseq, 'z))))
    eval(reader("(macro twolevel (x) `(macro secondlevel (y) `(- ~~x ~y)))", scope.symbolCache), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(twolevel 10)", scope.symbolCache), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(secondlevel 5)", scope.symbolCache), scope) shouldBe 5

    header("Multimethods and dispatch")
    scope(Symbol("mult-method")) = MM(List(Array(IntType)             -> F1({ (a : Int) => 666 }),
                                          Array(StringType)          -> F1({ (a : String) => "test" }),
                                          Array(StringType, IntType) -> F2({ (a : String, b : Int) => s"$a -> $b" })))
    eval(reader("(mult-method 1)", scope.symbolCache), scope) shouldBe 666
    eval(reader("(mult-method \"1\")", scope.symbolCache), scope) shouldBe "test"
    eval(reader("(mult-method \"test\" 2)", scope.symbolCache), scope) shouldBe "test -> 2"
    eval(reader("(mult-method 'test)", scope.symbolCache), scope) shouldThrow ArgumentError("No method matches prototype: (<native> <symbol>)")
    eval(reader("(string 'test)", scope.symbolCache), scope) shouldBe "test"
    eval(reader("(string 1)", scope.symbolCache), scope) shouldBe "1"
    eval(reader("(string 2.0)", scope.symbolCache), scope) shouldBe "2.0"
    eval(reader("(string [1 2 3])", scope.symbolCache), scope) shouldBe "[1 2 3]"
    eval(reader("(string #{1 2 3})", scope.symbolCache), scope) shouldBe "#{1 2 3}"
    eval(reader("(string {1 2 3 4})", scope.symbolCache), scope) shouldBe "{1 2 3 4}"
    eval(reader("(trim \"  b  \")", scope.symbolCache), scope) shouldBe "b"
    eval(reader("(substring \"  b  \" 2 3)", scope.symbolCache), scope) shouldBe "b"
    eval(reader("(loop 10 (lambda (x) (print 1 x)))", scope.symbolCache), scope).toString shouldBe "()"
    eval(reader("(defmethod lisp-mm (a <int> b <int>) (+ a b))", scope.symbolCache), scope)
    eval(reader("(defmethod lisp-mm (a <string> b <string>) (string-concat a b))", scope.symbolCache), scope)
    eval(reader("(lisp-mm 2 1)", scope.symbolCache), scope) shouldBe 3
    eval(reader("(lisp-mm \"Test\" \"1\")", scope.symbolCache), scope) shouldBe "Test1"
    eval(reader("(lisp-mm \"Test\" \"1\" 33)", scope.symbolCache), scope) shouldThrow ArgumentError("No method matches prototype: (lisp-mm <string> <string> <int>)")

    summary
  }
}
