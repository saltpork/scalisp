package scalisp.lib
import scalisp._

object `package` {
  // ------------------------------------------------------------------------------------------------------------
  // Basic bootstrap function environment
  // ------------------------------------------------------------------------------------------------------------
  val DefaultEnvironment = List(
      // Numbers
      Symbol("-<int>")      -> F2({ (a : Int, b : Int) => a - b }),
      Symbol("+<int>")      -> F2({ (a : Int, b : Int) => a + b }),
      Symbol("*<int>")      -> F2({ (a : Int, b : Int) => a * b }),
      Symbol("/<int>")      -> F2({ (a : Int, b : Int) => a / b }),
      Symbol("/<int>")      -> F2({ (a : Int, b : Int) => math.pow(a, b) }),
      Symbol("<<int>")      -> F2({ (a : Int, b : Int) => a < b }),
      Symbol("><int>")      -> F2({ (a : Int, b : Int) => a > b }),
      Symbol("+")           -> MM(Map(sig(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a + b }),
                                      sig(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a + b }),
                                      sig(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a + b }),
                                      sig(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a + b }),
                                      sig(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a + b }),
                                      sig(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a + b }),
                                      sig(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a + b }),
                                      sig(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a + b }),
                                      sig(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a + b })
                                  )),
      Symbol("-")           -> MM(Map(sig(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a - b }),
                                      sig(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a - b }),
                                      sig(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a - b }),
                                      sig(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a - b }),
                                      sig(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a - b }),
                                      sig(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a - b }),
                                      sig(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a - b }),
                                      sig(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a - b }),
                                      sig(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a - b })
                                  )),
      Symbol("*")           -> MM(Map(sig(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a * b }),
                                      sig(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a * b }),
                                      sig(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a * b }),
                                      sig(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a * b }),
                                      sig(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a * b }),
                                      sig(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a * b }),
                                      sig(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a * b }),
                                      sig(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a * b }),
                                      sig(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a * b })
                                  )),
      Symbol("/")           -> MM(Map(sig(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a / b }),
                                      sig(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a / b }),
                                      sig(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a / b }),
                                      sig(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a / b }),
                                      sig(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a / b }),
                                      sig(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a / b }),
                                      sig(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a / b }),
                                      sig(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a / b }),
                                      sig(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a / b })
                                  )),
      Symbol("**")          -> MM(Map(sig(IntType, IntType)       -> F2({ (a : Int, b : Int)       => math.pow(a, b) }),
                                      sig(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => math.pow(a, b) }),
                                      sig(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => math.pow(a, b) }),
                                      sig(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => math.pow(a, b) }),
                                      sig(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => math.pow(a, b) }),
                                      sig(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => math.pow(a, b) }),
                                      sig(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => math.pow(a, b) }),
                                      sig(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => math.pow(a, b) }),
                                      sig(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => math.pow(a, b) })
                                  )),
      Symbol("<")           -> MM(Map(sig(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a < b }),
                                      sig(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a < b }),
                                      sig(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a < b }),
                                      sig(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a < b }),
                                      sig(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a < b }),
                                      sig(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a < b }),
                                      sig(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a < b }),
                                      sig(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a < b }),
                                      sig(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a < b })
                                  )),
      Symbol(">")           -> MM(Map(sig(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a > b }),
                                      sig(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a > b }),
                                      sig(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a > b }),
                                      sig(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a > b }),
                                      sig(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a > b }),
                                      sig(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a > b }),
                                      sig(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a > b }),
                                      sig(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a > b }),
                                      sig(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a > b })
                                  )),
      Symbol("loop")        -> F2({ (n : Int, body : LFunc) =>
                                    body.scope = Scope(body.scope)
                                    for (i <- 0 until n) {
                                      body.scope(body.args(0)) = i
                                      body.f0
                                    }
                                  }),

      // Booleans
      Symbol("not")         -> F1({ (a : Boolean) => !a }),
      // and/or are macros

      // Collections
      Symbol("cons")        -> F2({ (a : Any, b : List[Any]) => a :: b }),
      Symbol("list")        -> Fn({ (xs : List[Any]) => List(xs : _*) }),
      Symbol("first")       -> F1({ (xs : Seq[Any]) => xs.head }),
      Symbol("rest")        -> F1({ (xs : Seq[Any]) => xs.tail }),

      // IO
      Symbol("print")       -> Fn({ (xs : List[Any]) => println(Console.YELLOW + xs.foldLeft("")(_ + _)) }),

      // Strings and symbols
      Symbol("string")      -> MM(Map(sig(SymbolType)  -> F1({ (a : Symbol)  => a.name })) ++
                                    (for (t <- 0x0 to 0xb if t != SymbolType) yield t.toLong -> F1({ (a : Any) => lispString(a) }))),
      Symbol("symbol")      -> F1({ (a : String) => Symbol(a) }),
      Symbol("trim")        -> F1({ (a : String) => a.trim }),
      Symbol("length")      -> F1({ (a : String) => a.length }),
      Symbol("lower-case")  -> F1({ (a : String) => a.toLowerCase }),
      Symbol("upper-case")  -> F1({ (a : String) => a.toUpperCase }),
      Symbol("string-concat") -> F2({ (a : String, b : String) => a + b }),
      Symbol("substring")   -> F3({ (a : String, start : Int, end : Int) => a.substring(start, end) }),
      Symbol("replace")     -> F3({ (a : String, find : String, replace : String) => a.replaceFirst(find, replace) }),
      Symbol("replace-all") -> F3({ (a : String, find : String, replace : String) => a.replaceAll(find, replace) })
    )
}
