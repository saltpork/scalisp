package scalisp.lib
import scalisp._

object `package` {
  // ------------------------------------------------------------------------------------------------------------
  // Basic bootstrap function environment
  // ------------------------------------------------------------------------------------------------------------
  val DefaultEnvironment = List(
      // Numbers
      Symbol("-<int>")      -> { (xs : List[Any]) => xs.head.asInstanceOf[Int] - xs.tail.head.asInstanceOf[Int] },
      Symbol("+<int>")      -> { (xs : List[Any]) => xs.head.asInstanceOf[Int] + xs.tail.head.asInstanceOf[Int] },
      Symbol("*<int>")      -> { (xs : List[Any]) => xs.head.asInstanceOf[Int] * xs.tail.head.asInstanceOf[Int] },
      Symbol("/<int>")      -> { (xs : List[Any]) => xs.head.asInstanceOf[Int] / xs.tail.head.asInstanceOf[Int] },
      Symbol("c+")          -> ({ case List((a : Int), (b : Int)) => a + b } : PartialFunction[List[Any], Any]),
      Symbol("+")           -> MultiMethod({ case (a : Int, b : Int) => a + b
                                             case (a : Int, b : Float) => a + b
                                             case (a : Int, b : Double) => a + b
                                             case (a : Double, b : Int) => a + b
                                             case (a : Double, b : Float) => a + b
                                             case (a : Double, b : Double) => a + b
                                             case (a : Float, b : Int) => a + b
                                             case (a : Float, b : Float) => a + b
                                             case (a : Float, b : Double) => a + b
                                           }),
      Symbol("*")           -> MultiMethod({ case (a : Int, b : Int) => a * b
                                             case (a : Int, b : Float) => a * b
                                             case (a : Int, b : Double) => a * b
                                             case (a : Double, b : Int) => a * b
                                             case (a : Double, b : Float) => a * b
                                             case (a : Double, b : Double) => a * b
                                             case (a : Float, b : Int) => a * b
                                             case (a : Float, b : Float) => a * b
                                             case (a : Float, b : Double) => a * b
                                           }),
      Symbol("/")           -> MultiMethod({ case (a : Int, b : Int) => a / b
                                             case (a : Int, b : Float) => a / b
                                             case (a : Int, b : Double) => a / b
                                             case (a : Double, b : Int) => a / b
                                             case (a : Double, b : Float) => a / b
                                             case (a : Double, b : Double) => a / b
                                             case (a : Float, b : Int) => a / b
                                             case (a : Float, b : Float) => a / b
                                             case (a : Float, b : Double) => a / b
                                           }),
      Symbol("-")           -> MultiMethod({ case (a : Int, b : Int) => a - b
                                             case (a : Int, b : Float) => a - b
                                             case (a : Int, b : Double) => a - b
                                             case (a : Double, b : Int) => a - b
                                             case (a : Double, b : Float) => a - b
                                             case (a : Double, b : Double) => a - b
                                             case (a : Float, b : Int) => a - b
                                             case (a : Float, b : Float) => a - b
                                             case (a : Float, b : Double) => a - b
                                           }),
      Symbol("**")          -> MultiMethod({ case (a : Int, b : Int) => math.pow(a, b)
                                             case (a : Int, b : Float) => math.pow(a, b)
                                             case (a : Int, b : Double) => math.pow(a, b)
                                             case (a : Double, b : Int) => math.pow(a, b)
                                             case (a : Double, b : Float) => math.pow(a, b)
                                             case (a : Double, b : Double) => math.pow(a, b)
                                             case (a : Float, b : Int) => math.pow(a, b)
                                             case (a : Float, b : Float) => math.pow(a, b)
                                             case (a : Float, b : Double) => math.pow(a, b)
                                           }),
      Symbol("loop")        -> { (xs : List[Any]) =>
        val n = xs(0).asInstanceOf[Int]
        val body = xs(1).asInstanceOf[Function1[List[Any], Any]]
        for (i <- 0 until n) body(List(i))
      },

      // Booleans
      Symbol("not")         -> MultiMethod({ case Tuple1(a : Boolean) => !a }),
      // and/or are macros

      // Collections
      Symbol("cons")        -> { (xs : List[Any]) => xs match {
                                  case a :: (b : List[Any]) :: Nil => a :: b
                                  case e @ _ => throw Error(s"cons called with illegal args: $e") }
      },
      Symbol("list")        -> { (xs : List[Any]) => List(xs : _*) },
      Symbol("first")       -> { (xs : List[Any]) => xs.head.asInstanceOf[Seq[Any]].head },
      Symbol("rest")        -> { (xs : List[Any]) => xs match {
                                  case (a : Seq[Any]) :: Nil => a.tail
                                  case e @ _ => throw Error(s"can't take rest of $e") }
      },

      // IO
      Symbol("print")       -> { (xs : List[Any]) => println(Console.YELLOW + xs.foldLeft("")(_ + _)) },

      // Strings and symbols
      Symbol("string")      -> MultiMethod({ case Tuple1(Symbol(b)) => b; case Tuple1(a @ _) => lispString(a) }),
      Symbol("symbol")      -> MultiMethod({ case Tuple1(a : String) => Symbol(a) }),
      Symbol("trim")        -> MultiMethod({ case Tuple1(a : String) => a.trim }),
      Symbol("length")      -> MultiMethod({ case Tuple1(a : String) => a.trim }),
      Symbol("lower-case")  -> MultiMethod({ case Tuple1(a : String) => a.toLowerCase }),
      Symbol("upper-case")  -> MultiMethod({ case Tuple1(a : String) => a.toUpperCase }),
      Symbol("substring")   -> MultiMethod({ case (a : String, start : Int, end : Int) => a.substring(start, end) }),
      Symbol("replace")     -> MultiMethod({ case (a : String, find : String, replace : String) => a.replace(find, replace) }),
      Symbol("replace-all") -> MultiMethod({ case (a : String, find : String, replace : String) => a.replaceAll(find, replace) })
    )
}
