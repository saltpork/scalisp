package scalisp.lib
import scalisp._

object `package` {
  // ------------------------------------------------------------------------------------------------------------
  // Basic bootstrap function environment
  // ------------------------------------------------------------------------------------------------------------
  val DefaultEnvironment = List(
      // Numbers
      Sym("-<int>")        -> F2({ (a : Int, b : Int) => a - b }),
      Sym("+<int>")        -> F2({ (a : Int, b : Int) => a + b }),
      Sym("*<int>")        -> F2({ (a : Int, b : Int) => a * b }),
      Sym("/<int>")        -> F2({ (a : Int, b : Int) => a / b }),
      Sym("/<int>")        -> F2({ (a : Int, b : Int) => math.pow(a, b) }),
      Sym("<<int>")        -> F2({ (a : Int, b : Int) => a < b }),
      Sym("><int>")        -> F2({ (a : Int, b : Int) => a > b }),
      Sym("+")             -> MM(List(Array(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a + b }),
                                      Array(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a + b }),
                                      Array(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a + b }),
                                      Array(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a + b }),
                                      Array(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a + b }),
                                      Array(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a + b }),
                                      Array(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a + b }),
                                      Array(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a + b }),
                                      Array(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a + b })
                                 )),
      Sym("-")             -> MM(List(Array(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a - b }),
                                      Array(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a - b }),
                                      Array(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a - b }),
                                      Array(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a - b }),
                                      Array(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a - b }),
                                      Array(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a - b }),
                                      Array(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a - b }),
                                      Array(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a - b }),
                                      Array(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a - b })
                                 )),
      Sym("*")             -> MM(List(Array(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a * b }),
                                      Array(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a * b }),
                                      Array(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a * b }),
                                      Array(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a * b }),
                                      Array(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a * b }),
                                      Array(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a * b }),
                                      Array(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a * b }),
                                      Array(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a * b }),
                                      Array(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a * b })
                                 )),
      Sym("/")             -> MM(List(Array(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a / b }),
                                      Array(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a / b }),
                                      Array(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a / b }),
                                      Array(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a / b }),
                                      Array(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a / b }),
                                      Array(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a / b }),
                                      Array(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a / b }),
                                      Array(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a / b }),
                                      Array(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a / b })
                                 )),
      Sym("**")            -> MM(List(Array(IntType, IntType)       -> F2({ (a : Int, b : Int)       => math.pow(a, b) }),
                                      Array(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => math.pow(a, b) }),
                                      Array(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => math.pow(a, b) }),
                                      Array(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => math.pow(a, b) }),
                                      Array(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => math.pow(a, b) }),
                                      Array(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => math.pow(a, b) }),
                                      Array(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => math.pow(a, b) }),
                                      Array(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => math.pow(a, b) }),
                                      Array(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => math.pow(a, b) })
                                 )),
      Sym("<")             -> MM(List(Array(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a < b }),
                                      Array(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a < b }),
                                      Array(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a < b }),
                                      Array(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a < b }),
                                      Array(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a < b }),
                                      Array(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a < b }),
                                      Array(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a < b }),
                                      Array(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a < b }),
                                      Array(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a < b })
                                 )),
      Sym(">")             -> MM(List(Array(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a > b }),
                                      Array(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a > b }),
                                      Array(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a > b }),
                                      Array(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a > b }),
                                      Array(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a > b }),
                                      Array(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a > b }),
                                      Array(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a > b }),
                                      Array(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a > b }),
                                      Array(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a > b })
                               )),
      Sym("loop")          -> F2({ (n : Int, body : LFunc) =>
                                   body.scope = Scope(body.scope)
                                   body.scope(body.argNames(0)) = 0
                                   var i = 0
                                   //for (i <- 0 until n) {
                                   while(i < n) {
                                     body.scope(body.argNames(0).index) = i
                                     body.f0
                                     i += 1
                                   }
                                 }),

      // Booleans
      Sym("not")           -> F1({ (a : Boolean) => !a }),
      // and/or are macros

      // Collections
      Sym("cons")          -> F2({ (a : Any, b : Array[Any]) => Array(a) ++ b }),
      Sym("list")          -> Fn({ (xs : Array[Any]) => Array(xs : _*) }),
      Sym("first")         -> F1({ case (xs : Array[Any]) => xs(0); case (xs : Seq[Any]) => xs.head } : PartialFunction[Any, Any]),
      Sym("rest")          -> F1({ case (xs : Array[Any]) => xs.drop(1); case (xs : Seq[Any]) => xs.tail } : PartialFunction[Any, Any]),

      // IO
      Sym("print")         -> Fn({ (xs : Array[Any]) => println(Console.YELLOW + xs.foldLeft("")(_ + _)) }),

      // Strings and symbols
      //Sym("string")      -> MM(List(Array(SymType)  -> F1({ (a : Sym) a.name })) ++
      Sym("string")        -> MM(List(Array(SymbolType)  -> F1({ (a : Sym) => a.name } )) ++
                                   (for (t <- 0x0 to 0xb if t != SymbolType) yield Array(t.toByte) -> F1({ (a : Any) => lispString(a) }))),
      Sym("symbol")        -> F1({ (a : String) => Sym(a) }),
      Sym("trim")          -> F1({ (a : String) => a.trim }),
      Sym("length")        -> F1({ (a : String) => a.length }),
      Sym("lower-case")    -> F1({ (a : String) => a.toLowerCase }),
      Sym("upper-case")    -> F1({ (a : String) => a.toUpperCase }),
      Sym("string-concat") -> F2({ (a : String, b : String) => a + b }),
      Sym("substring")     -> F3({ (a : String, start : Int, end : Int) => a.substring(start, end) }),
      Sym("replace")       -> F3({ (a : String, find : String, replace : String) => a.replaceFirst(find, replace) }),
      Sym("replace-all")   -> F3({ (a : String, find : String, replace : String) => a.replaceAll(find, replace) })
    )
}
