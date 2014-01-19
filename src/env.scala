package scalisp.lib
import scalisp._

object `package` {
  // ------------------------------------------------------------------------------------------------------------
  // Basic bootstrap function environment
  // ------------------------------------------------------------------------------------------------------------
  val DefaultEnvironment = List(
      // Numbers
      Sym("-<int>")        -> new F2n[Int, Int, Int] { def apply(a : Int, b : Int) = a - b },
      Sym("+<int>")        -> new F2n[Int, Int, Int] { def apply(a : Int, b : Int) = a + b },
      Sym("*<int>")        -> new F2n[Int, Int, Int] { def apply(a : Int, b : Int) = a * b },
      Sym("/<int>")        -> new F2n[Int, Int, Int] { def apply(a : Int, b : Int) = a / b },
      Sym("**<int>")       -> new F2n[Int, Int, Double] { def apply(a : Int, b : Int) = math.pow(a, b) },
      Sym("<<int>")        -> new F2n[Int, Int, Boolean] { @inline def apply(a : Int, b : Int) = a < b },
      Sym("><int>")        -> new F2n[Int, Int, Boolean] { def apply(a : Int, b : Int) = a > b },
      Sym("+")             -> MM(List(Array(IntType, IntType)       -> new F2n[Int, Int, Int]          { def apply(a : Int, b : Int)       = a + b },
                                      Array(IntType, FloatType)     -> new F2n[Int, Float, Float]      { def apply(a : Int, b : Float)     = a + b },
                                      Array(IntType, DoubleType)    -> new F2n[Int, Double, Double]    { def apply(a : Int, b : Double)    = a + b },
                                      Array(DoubleType, IntType)    -> new F2n[Double, Int, Double]    { def apply(a : Double, b : Int)    = a + b },
                                      Array(DoubleType, FloatType)  -> new F2n[Double, Float, Double]  { def apply(a : Double, b : Float)  = a + b },
                                      Array(DoubleType, DoubleType) -> new F2n[Double, Double, Double] { def apply(a : Double, b : Double) = a + b },
                                      Array(FloatType, IntType)     -> new F2n[Float, Int, Float]      { def apply(a : Float, b : Int)     = a + b },
                                      Array(FloatType, FloatType)   -> new F2n[Float, Float, Float]    { def apply(a : Float, b : Float)   = a + b },
                                      Array(FloatType, DoubleType)  -> new F2n[Float, Double, Double]  { def apply(a : Float, b : Double)  = a + b }
                                 )),
      Sym("-")             -> MM(List(Array(IntType, IntType)       -> new F2n[Int, Int, Int]          { def apply(a : Int, b : Int)       = a - b },
                                      Array(IntType, FloatType)     -> new F2n[Int, Float, Float]      { def apply(a : Int, b : Float)     = a - b },
                                      Array(IntType, DoubleType)    -> new F2n[Int, Double, Double]    { def apply(a : Int, b : Double)    = a - b },
                                      Array(DoubleType, IntType)    -> new F2n[Double, Int, Double]    { def apply(a : Double, b : Int)    = a - b },
                                      Array(DoubleType, FloatType)  -> new F2n[Double, Float, Double]  { def apply(a : Double, b : Float)  = a - b },
                                      Array(DoubleType, DoubleType) -> new F2n[Double, Double, Double] { def apply(a : Double, b : Double) = a - b },
                                      Array(FloatType, IntType)     -> new F2n[Float, Int, Float]      { def apply(a : Float, b : Int)     = a - b },
                                      Array(FloatType, FloatType)   -> new F2n[Float, Float, Float]    { def apply(a : Float, b : Float)   = a - b },
                                      Array(FloatType, DoubleType)  -> new F2n[Float, Double, Double]  { def apply(a : Float, b : Double)  = a - b }
                                 )),
      Sym("*")             -> MM(List(Array(IntType, IntType)       -> new F2n[Int, Int, Int]          { def apply(a : Int, b : Int)       = a * b },
                                      Array(IntType, FloatType)     -> new F2n[Int, Float, Float]      { def apply(a : Int, b : Float)     = a * b },
                                      Array(IntType, DoubleType)    -> new F2n[Int, Double, Double]    { def apply(a : Int, b : Double)    = a * b },
                                      Array(DoubleType, IntType)    -> new F2n[Double, Int, Double]    { def apply(a : Double, b : Int)    = a * b },
                                      Array(DoubleType, FloatType)  -> new F2n[Double, Float, Double]  { def apply(a : Double, b : Float)  = a * b },
                                      Array(DoubleType, DoubleType) -> new F2n[Double, Double, Double] { def apply(a : Double, b : Double) = a * b },
                                      Array(FloatType, IntType)     -> new F2n[Float, Int, Float]      { def apply(a : Float, b : Int)     = a * b },
                                      Array(FloatType, FloatType)   -> new F2n[Float, Float, Float]    { def apply(a : Float, b : Float)   = a * b },
                                      Array(FloatType, DoubleType)  -> new F2n[Float, Double, Double]  { def apply(a : Float, b : Double)  = a * b }
                                 )),
      Sym("/")             -> MM(List(Array(IntType, IntType)       -> new F2n[Int, Int, Int]          { def apply(a : Int, b : Int)       = a / b },
                                      Array(IntType, FloatType)     -> new F2n[Int, Float, Float]      { def apply(a : Int, b : Float)     = a / b },
                                      Array(IntType, DoubleType)    -> new F2n[Int, Double, Double]    { def apply(a : Int, b : Double)    = a / b },
                                      Array(DoubleType, IntType)    -> new F2n[Double, Int, Double]    { def apply(a : Double, b : Int)    = a / b },
                                      Array(DoubleType, FloatType)  -> new F2n[Double, Float, Double]  { def apply(a : Double, b : Float)  = a / b },
                                      Array(DoubleType, DoubleType) -> new F2n[Double, Double, Double] { def apply(a : Double, b : Double) = a / b },
                                      Array(FloatType, IntType)     -> new F2n[Float, Int, Float]      { def apply(a : Float, b : Int)     = a / b },
                                      Array(FloatType, FloatType)   -> new F2n[Float, Float, Float]    { def apply(a : Float, b : Float)   = a / b },
                                      Array(FloatType, DoubleType)  -> new F2n[Float, Double, Double] {  def apply(a : Float, b : Double)  = a / b }
                                 )),
      Sym("**")            -> MM(List(Array(IntType, IntType)       -> new F2n[Int, Int, Double]       { def apply(a : Int, b : Int)       = math.pow(a, b) },
                                      Array(IntType, FloatType)     -> new F2n[Int, Float, Double]     { def apply(a : Int, b : Float)     = math.pow(a, b) },
                                      Array(IntType, DoubleType)    -> new F2n[Int, Double, Double]    { def apply(a : Int, b : Double)    = math.pow(a, b) },
                                      Array(DoubleType, IntType)    -> new F2n[Double, Int, Double]    { def apply(a : Double, b : Int)    = math.pow(a, b) },
                                      Array(DoubleType, FloatType)  -> new F2n[Double, Float, Double]  { def apply(a : Double, b : Float)  = math.pow(a, b) },
                                      Array(DoubleType, DoubleType) -> new F2n[Double, Double, Double] { def apply(a : Double, b : Double) = math.pow(a, b) },
                                      Array(FloatType, IntType)     -> new F2n[Float, Int, Double]     { def apply(a : Float, b : Int)     = math.pow(a, b) },
                                      Array(FloatType, FloatType)   -> new F2n[Float, Float, Double]   { def apply(a : Float, b : Float)   = math.pow(a, b) },
                                      Array(FloatType, DoubleType)  -> new F2n[Float, Double, Double]  { def apply(a : Float, b : Double)  = math.pow(a, b) }
                                 )),
      Sym("<")             -> MM(List(Array(IntType, IntType)       -> new F2n[Int, Int, Boolean]       { def apply(a : Int, b : Int)       = a < b },
                                      Array(IntType, FloatType)     -> new F2n[Int, Float, Boolean]     { def apply(a : Int, b : Float)     = a < b },
                                      Array(IntType, DoubleType)    -> new F2n[Int, Double, Boolean]    { def apply(a : Int, b : Double)    = a < b },
                                      Array(DoubleType, IntType)    -> new F2n[Double, Int, Boolean]    { def apply(a : Double, b : Int)    = a < b },
                                      Array(DoubleType, FloatType)  -> new F2n[Double, Float, Boolean]  { def apply(a : Double, b : Float)  = a < b },
                                      Array(DoubleType, DoubleType) -> new F2n[Double, Double, Boolean] { def apply(a : Double, b : Double) = a < b },
                                      Array(FloatType, IntType)     -> new F2n[Float, Int, Boolean]     { def apply(a : Float, b : Int)     = a < b },
                                      Array(FloatType, FloatType)   -> new F2n[Float, Float, Boolean]   { def apply(a : Float, b : Float)   = a < b },
                                      Array(FloatType, DoubleType)  -> new F2n[Float, Double, Boolean]  { def apply(a : Float, b : Double)  = a < b }
                                 )),
      Sym(">")             -> MM(List(Array(IntType, IntType)       -> new F2n[Int, Int, Boolean]       { def apply(a : Int, b : Int)       = a > b },
                                      Array(IntType, FloatType)     -> new F2n[Int, Float, Boolean]     { def apply(a : Int, b : Float)     = a > b },
                                      Array(IntType, DoubleType)    -> new F2n[Int, Double, Boolean]    { def apply(a : Int, b : Double)    = a > b },
                                      Array(DoubleType, IntType)    -> new F2n[Double, Int, Boolean]    { def apply(a : Double, b : Int)    = a > b },
                                      Array(DoubleType, FloatType)  -> new F2n[Double, Float, Boolean]  { def apply(a : Double, b : Float)  = a > b },
                                      Array(DoubleType, DoubleType) -> new F2n[Double, Double, Boolean] { def apply(a : Double, b : Double) = a > b },
                                      Array(FloatType, IntType)     -> new F2n[Float, Int, Boolean]     { def apply(a : Float, b : Int)     = a > b },
                                      Array(FloatType, FloatType)   -> new F2n[Float, Float, Boolean]   { def apply(a : Float, b : Float)   = a > b },
                                      Array(FloatType, DoubleType)  -> new F2n[Float, Double, Boolean]  { def apply(a : Float, b : Double)  = a > b }
                                 )),
      // Sym("-")             -> MM(List(Array(IntType, IntType)       -> F2({ (a : Int, b : Int)       => a - b }),
      //                                 Array(IntType, FloatType)     -> F2({ (a : Int, b : Float)     => a - b }),
      //                                 Array(IntType, DoubleType)    -> F2({ (a : Int, b : Double)    => a - b }),
      //                                 Array(DoubleType, IntType)    -> F2({ (a : Double, b : Int)    => a - b }),
      //                                 Array(DoubleType, FloatType)  -> F2({ (a : Double, b : Float)  => a - b }),
      //                                 Array(DoubleType, DoubleType) -> F2({ (a : Double, b : Double) => a - b }),
      //                                 Array(FloatType, IntType)     -> F2({ (a : Float, b : Int)     => a - b }),
      //                                 Array(FloatType, FloatType)   -> F2({ (a : Float, b : Float)   => a - b }),
      //                                 Array(FloatType, DoubleType)  -> F2({ (a : Float, b : Double)  => a - b })
      //                            )),
      Sym("loop")          -> new F2n[Int, LFunc, Any] {
        def apply(n : Int, body : LFunc) = {
          body.scope = Scope(body.scope)
          body.scope(body.argNames(0)) = 0
          var i = 0
          //for (i <- 0 until n) {
          while(i < n) {
            body.scope(body.argNames(0).index) = i
            body.f0
            i += 1
          }
        } },

      // Booleans
      Sym("not")           -> new F1n[Boolean, Boolean] { def apply(a : Boolean) = !a },
      // and/or are macros

      // Collections
      Sym("cons")          -> new F2n[Any, Array[Any], Array[Any]] { def apply(a : Any, b : Array[Any]) = Array(a) ++ b },
      Sym("list")          -> new Fn[Array[Any]] { def apply(xs : Array[Any]) = Array(xs : _*) },
      Sym("first")         -> new F1n[AnyRef, Any] { def apply(xs : AnyRef) = xs match {
                                                        case xs : Array[Any] => xs(0)
                                                        case xs : Seq[Any] => xs.head } },
      Sym("rest")          -> new F1n[AnyRef, Any] { def apply(xs : AnyRef) = xs match {
                                                      case xs : Array[Any] => xs.drop(1)
                                                      case xs : Seq[Any] => xs.tail } },

      // IO
      Sym("print")         -> new Fn[Unit] { def apply(xs : Array[Any]) = println(Console.YELLOW + xs.foldLeft("")(_ + _)) },

      // Strings and symbols
      //Sym("string")      -> MM(List(Array(SymType)  -> F1({ (a : Sym) a.name })) ++
      Sym("string")        -> MM(List(Array(SymbolType)  -> new F1n[Sym, String] { def apply(a : Sym) = a.name }) ++
                                   (for (t <- 0x0 to 0xb if t != SymbolType) yield Array(t.toByte) -> new F1n[Any, String] { def apply(a : Any) = lispString(a) })),
      Sym("symbol")        -> new F1n[String, Sym] { def apply(a : String) = Sym(a) },
      Sym("trim")          -> new F1n[String, String] { def apply(a : String) = a.trim },
      Sym("length")        -> new F1n[String, Int] { def apply(a : String) = a.length },
      Sym("lower-case")    -> new F1n[String, String] { def apply(a : String) = a.toLowerCase },
      Sym("upper-case")    -> new F1n[String, String] { def apply(a : String) = a.toUpperCase },
      Sym("string-concat") -> new F2n[String, String, String] { def apply(a : String, b : String) = a + b },
      Sym("substring")     -> new F3n[String, Int, Int, String] { def apply(a : String, start : Int, end : Int) = a.substring(start, end) },
      Sym("replace")       -> new F3n[String, String, String, String] { def apply(a : String, find : String, replace : String) = a.replaceFirst(find, replace) },
      Sym("replace-all")   -> new F3n[String, String, String, String] { def apply(a : String, find : String, replace : String) = a.replaceAll(find, replace) }
    )
}
