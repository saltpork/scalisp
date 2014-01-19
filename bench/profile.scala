package scabench
import scalisp._

object Profile extends {
  def time(name : String)(a : => Any) = {
    val start = System.nanoTime
    val res   = a
    val dur   = System.nanoTime - start
    println(f"${name + ":"}%-60s ${dur / 1e9}%20.6f (result: $res)")
    dur.toDouble -> res
  }

  def main(args : Array[String]) {
    val reader  = new Reader
    val scope   = Scope(lib.DefaultEnvironment : _*)

    eval(reader("(set! fibo (lambda (x) (if (< x 2) 1 (+ (fibo (- x 1)) (fibo (- x 2))))))"), scope)
    for (size <- List(40)) {
      val ast = reader(s"(fibo $size)")
      for (i <- 0 until 3) {
        time(s"FIBO [iteration: $i, size: $size]") {
          eval(ast, scope)
        }
      }
    }

    eval(reader("(set! fibo2 (lambda (x) (if (<<int> x 2) 1 (+<int> (fibo2 (-<int> x 1)) (fibo2 (-<int> x 2))))))"), scope)
    for (size <- List(40)) {
      val ast = reader(s"(fibo2 $size)")
      for (i <- 0 until 3) {
        time(s"FIBO INT Specialized [iteration: $i, size: $size]") {
          eval(ast, scope)
        }
      }
    }
  }
}
