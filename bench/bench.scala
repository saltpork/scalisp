package scabench
import scalisp.{Scope => sScope, _}
import org.scalameter.api._

object DispatchPerformance extends PerformanceTest.Quickbenchmark {
  val reader  = new Reader
  val scope   = sScope(lib.DefaultEnvironment : _*)

  val iters  = Gen.enumeration("iterations")(100000, 1000000)

  val sizes  = Gen.range("iteration")(1, 10, 1)
  val ranges = for (size <- sizes) yield size

   performance of "Native lambda dispatch" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))")
       eval(reader("(set! x 0)"), scope)
       var x = 0
       val fn = { (x : Int, y : Int) => x + y }

       for (i <- 0 until iterations) x = fn(x, 1)
     }
   }

   performance of "Native partial function dispatch" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))")
       eval(reader("(set! x 0)"), scope)
       var x = 0
       val fn = { case (x : Int, y : Int) => x + y } : PartialFunction[(Int, Int), Int]

       for (i <- 0 until iterations) x = fn((x, 1))
     }
   }

   performance of "Native partial function dispatch + store into map" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))")
       eval(reader("(set! x 0)"), scope)
       val map = collection.mutable.Map("x" -> 0)
       val fn = { case (x : Int, y : Int) => x + y } : PartialFunction[(Int, Int), Int]

       for (i <- 0 until iterations) map("x") = fn((map("x"), 1))
     }
   }

   performance of "Native list dispatch + store into map" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))")
       eval(reader("(set! x 0)"), scope)
       val fn = { (x : List[Any]) => x(0).asInstanceOf[Int] + x(1).asInstanceOf[Int] }
       val map = collection.mutable.Map("x" -> 0, "fn" -> fn)

       for (i <- 0 until iterations) map("x") = map("fn").asInstanceOf[Function1[List[Any], Any]](List(map("x"), 1))
     }
   }

   performance of "Single dispatch" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+<int> x 1))")
       eval(reader("(set! x 0)"), scope)

       for (i <- 0 until iterations) eval(ast, scope)
     }
   }

   performance of "Partial function function dispatch" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (c+ x 1))")
       eval(reader("(set! x 0)"), scope)

       for (i <- 0 until iterations) eval(ast, scope)
     }
   }

   performance of "Multiple dispatch" in {
     using(iters) in { iterations =>
       val ast = reader("(set! x (+ x 1))")
       eval(reader("(set! x 0)"), scope)

       for (i <- 0 until iterations) eval(ast, scope)
     }
   }

   performance of "Loop with single dispatch (1e5 iterations)" in {
     val ast = reader("(loop 100000 (lambda (y) (set! x (+<int> x y))))")
     eval(reader("(set! x 0)"), scope)

     using(ranges) in { iteration => eval(ast, scope) }
   }

   performance of "Loop with multiple dispatch (1e5 iterations)" in {
     val ast = reader("(loop 100000 (lambda (y) (set! x (+ x y))))")
     eval(reader("(set! x 0)"), scope)

     using(ranges) in { iteration => eval(ast, scope) }
   }

   performance of "Fibo" in {
     eval(reader("(set! fibo (lambda (x) (if (< x 2) 1 (+ (fibo (- x 1)) (fibo (- x 2))))))"), scope)
     val start = System.nanoTime
     val ast = reader("(fibo 40)")

     println("FIBO: " + eval(ast, scope))
     val dur = System.nanoTime - start
     println(s"FIBO: ${dur / 1e9}")
     using(ranges) in { iteration => eval(ast, scope) }
   }
}
