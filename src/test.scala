package scalisp

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

    scope('function) = { (xs : List[Any]) => xs.foldLeft(0.0f)(_ + _.asInstanceOf[Float]) }

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

    header("Reading with reader macros")
    reader("#{a b c}") shouldBe Set('a, 'b, 'c)
    reader("'(a b c)") shouldBe List('quote, List('a, 'b, 'c))
    reader("{a b c d}") shouldBe Map('a -> 'b, 'c -> 'd)
    reader("[a b c d]") shouldBe Vector('a, 'b, 'c, 'd)
    reader("[a #{ b b } (c c) [d d]]") shouldBe Vector('a, Set('b), List('c, 'c), Vector('d, 'd))
    reader("#{{a a b b} [b b] #{c d}}") shouldBe Set(Map('a -> 'a, 'b -> 'b), Vector('b, 'b), Set('c, 'd))
    reader("`(test ~x ~@y)") shouldBe List('quasiquote, List('test, List('splice, 'x), List('spliceseq, 'y)))

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

    header("Read and evaluate")
    eval(reader("(function 2.2 3.3 (function 1.0 2.0))"), scope) shouldBe 8.5f
    eval(reader("((lambda (x) (+ x 1)) 1)"), scope) shouldBe 2
    eval(reader("`(test ~y ~y)"), scope) shouldBe List('test, 4, 4)
    eval(reader("(set! z '(3 2 1))"), scope) shouldBe List(3, 2, 1)
    eval(reader("{ y y }"), scope) shouldBe Map(4 -> 4)
    eval(reader("{ y y 1 z }"), scope) shouldBe Map(4 -> 4, 1 -> List(3, 2, 1))
    eval(reader("[ y y { 1 z } ]"), scope) shouldBe Vector(4, 4, Map(1 -> List(3, 2, 1)))
    eval(reader("#{(incr 10) y}"), scope) shouldBe Set(11, 4)
    eval(reader("(first '(1 2 3))"), scope) shouldBe 1
    eval(reader("(rest '(1 2 3))"), scope) shouldBe List(2, 3)
    eval(reader("(first [1 2 3])"), scope) shouldBe 1
    eval(reader("(rest [1 2 3])"), scope) shouldBe Vector(2, 3)

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
    eval(reader("(macro def2 (name args body) `(def ~name (lambda ~args ~body)))"), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(def2 addxy2 (x y) (+ x y))"), scope).toString shouldBe "<function1>"
    eval(reader("(addxy2 1 3)"), scope) shouldBe 4
    eval(reader("`(test ~y ~@z)"), scope) shouldBe List('test, 4, 3, 2, 1)
    eval(reader("`{ ~y ~y }"), scope) shouldBe Map(4 -> 4)
    eval(reader("((lambda (y) `(print `(+ ~~y ~@z))) 7)"), scope) shouldBe List('print, List('quasiquote, List('+, 7, List('spliceseq, 'z))))
    eval(reader("(macro twolevel (x) `(macro secondlevel (y) `(- ~~x ~y)))"), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(twolevel 10)"), scope).toString shouldBe "Macro(<function1>)"
    eval(reader("(secondlevel 5)"), scope) shouldBe 5

    header("Multimethods and dispatch")
    scope(Symbol("mult-method")) = MultiMethod({ case Tuple1(a : Int) => 666 }, { case Tuple1(a : String) => "test" }, { case Tuple2(a : String, b : Int) => s"$a -> $b" })
    eval(reader("(mult-method 1)"), scope) shouldBe 666
    eval(reader("(mult-method \"1\")"), scope) shouldBe "test"
    eval(reader("(mult-method \"test\" 2)"), scope) shouldBe "test -> 2"
    eval(reader("(mult-method 'test)"), scope) shouldThrow ArgumentError("No method matches ('test)")
    eval(reader("(string 'test)"), scope) shouldBe "test"
    eval(reader("(string 1)"), scope) shouldBe "1"
    eval(reader("(string 2.0)"), scope) shouldBe "2.0"
    eval(reader("(string [1 2 3])"), scope) shouldBe "[1 2 3]"
    eval(reader("(string #{1 2 3})"), scope) shouldBe "#{1 2 3}"
    eval(reader("(string {1 2 3 4})"), scope) shouldBe "{1 2 3 4}"
    eval(reader("(trim \"  b  \")"), scope) shouldBe "b"
    eval(reader("(substring \"  b  \" 2 3)"), scope) shouldBe "b"
    eval(reader("(loop 10 (lambda (x) (print 1 x)))"), scope).toString shouldBe "()"

    summary
  }
}