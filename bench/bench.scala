package scabench
import scalisp.{Scope => sScope, _}
import org.scalameter.api._

object DispatchPerformance extends PerformanceTest.Quickbenchmark {
  def time(name : String)(a : => Any) = {
    val start = System.nanoTime
    val res   = a
    val dur   = System.nanoTime - start
    println(f"${name + ":"}%-30s ${dur / 1e9}%20.6f (result: $res)")
    dur.toDouble -> res
  }

  val reader  = new Reader
  val scope   = sScope(lib.DefaultEnvironment : _*)

  val iters  = Gen.enumeration("iterations")(100000, 1000000)

  val sizes  = Gen.range("iteration")(1, 5, 1)
  val ranges = for (size <- sizes) yield size

   performance of "Native lambda dispatch" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))", scope.symbolCache)
       eval(reader("(set! x 0)", scope.symbolCache), scope)
       var x = 0
       val fn = { (x : Int, y : Int) => x + y }

       for (i <- 0 until iterations) x = fn(x, 1)
     }
   }

   performance of "Native partial function dispatch" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))", scope.symbolCache)
       eval(reader("(set! x 0)", scope.symbolCache), scope)
       var x = 0
       val fn = { case (x : Int, y : Int) => x + y } : PartialFunction[(Int, Int), Int]

       for (i <- 0 until iterations) x = fn((x, 1))
     }
   }

   performance of "Native partial function dispatch + store into map" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))", scope.symbolCache)
       eval(reader("(set! x 0)", scope.symbolCache), scope)
       val map = collection.mutable.Map("x" -> 0)
       val fn = { case (x : Int, y : Int) => x + y } : PartialFunction[(Int, Int), Int]

       for (i <- 0 until iterations) map("x") = fn((map("x"), 1))
     }
   }

   performance of "Native list dispatch + store into map" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))", scope.symbolCache)
       eval(reader("(set! x 0)", scope.symbolCache), scope)
       val fn = { (x : List[Any]) => x(0).asInstanceOf[Int] + x(1).asInstanceOf[Int] }
       val map = collection.mutable.Map("x" -> 0, "fn" -> fn)

       for (i <- 0 until iterations) map("x") = map("fn").asInstanceOf[Function1[List[Any], Any]](List(map("x"), 1))
     }
   }

   performance of "Single dispatch" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))", scope.symbolCache)
       eval(reader("(set! x 0)", scope.symbolCache), scope)

       for (i <- 0 until iterations) eval(ast, scope)
     }
   }

   performance of "Multiple dispatch" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+ x 1))", scope.symbolCache)
       eval(reader("(set! x 0)", scope.symbolCache), scope)

       for (i <- 0 until iterations) eval(ast, scope)
     }
   }

   performance of "Loop with single dispatch (1e5 iterations)" in {
     val ast = reader("(loop 100000 (lambda (y) (set! x (+<int> x y))))", scope.symbolCache)
     eval(reader("(set! x 0)", scope.symbolCache), scope)

     using(ranges) in { iteration => eval(ast, scope) }
   }

   performance of "Loop with multiple dispatch (1e5 iterations)" in {
     val ast = reader("(loop 100000 (lambda (y) (set! x (+ x y))))", scope.symbolCache)
     eval(reader("(set! x 0)", scope.symbolCache), scope)

     using(ranges) in { iteration => eval(ast, scope) }
   }

  for (size <- List(20, 30)) {
    performance of s"Fibo $size (native multimethod)" in {
      eval(reader("(set! fibo (lambda (x) (if (< x 2) 1 (+ (fibo (- x 1)) (fibo (- x 2))))))", scope.symbolCache), scope)
      val ast = reader(s"(fibo $size)", scope.symbolCache)

      time(s"FIBO $size") {
        eval(ast, scope)
      }
      using(ranges) in { iteration => eval(ast, scope) }
    }
    performance of s"Fibo $size (native int speciialized)" in {
      eval(reader("(set! fibo2 (lambda (x) (if (<<int> x 2) 1 (+<int> (fibo2 (-<int> x 1)) (fibo2 (-<int> x 2))))))", scope.symbolCache), scope)
      val ast = reader(s"(fibo2 $size)", scope.symbolCache)

      time(s"FIBO $size [INT]") {
        eval(ast, scope)
      }
      using(ranges) in { iteration => eval(ast, scope) }
    }
  }
}
