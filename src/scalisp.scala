package scalisp

import util.parsing.combinator.{RegexParsers, PackratParsers}
import collection.mutable.{Map => mMap, HashMap, ArrayBuffer}
import java.io.{Reader => jReader}
import tools.jline.console.ConsoleReader
import tools.jline.console.history.FileHistory
import language.dynamics
import annotation.tailrec

object `package` {
  def mapFromList(list : List[Any]) = Map(list.grouped(2).map { case List(a, b) => a -> b }.toList : _*)

  final val Lambda     = Symbol("lambda")
  final val Quote      = Symbol("quote")
  final val Quasiquote = Symbol("quasiquote")
  final val Splice     = Symbol("splice")
  final val SpliceSeq  = Symbol("spliceseq")
  final val SetBang    = Symbol("set!")
  final val Def        = Symbol("def")
  final val MacroSym   = Symbol("macro")
  final val If         = Symbol("if")
  final val DefMethod  = Symbol("defmethod")
  final val Paste      = Symbol("paste")

  final val SymbolType : Byte   = 0x1 // Non-numbers
  final val StringType : Byte   = 0x2
  final val CharType : Byte     = 0x3
  final val ShortType : Byte    = 0x4 // Numbers
  final val IntType : Byte      = 0x5
  final val FloatType : Byte    = 0x6
  final val DoubleType : Byte   = 0x7
  final val FunctionType : Byte = 0x8
  final val SetType : Byte      = 0x9 // Collections
  final val VectorType : Byte   = 0xa
  final val MapType : Byte      = 0xb
  final val SType : Byte        = 0xc
  final val AnyType : Byte      = 0xf

  final val ZeroArity = -1L

  final val typeName = Map[Byte, String](ShortType    -> "<short>",
                                         IntType      -> "<int>",
                                         FloatType    -> "<float>",
                                         DoubleType   -> "<double>",
                                         SymbolType   -> "<symbol>",
                                         StringType   -> "<string>",
                                         CharType     -> "<char>",
                                         FunctionType -> "<function>",
                                         SetType      -> "<set>",
                                         VectorType   -> "<vector>",
                                         MapType      -> "<map>",
                                         SType        -> "<list>",
                                         AnyType      -> "<any>"
                                         )
  final val nameType = Map[Symbol, Byte](Symbol("<short>")    -> ShortType,
                                         Symbol("<int>")      -> IntType,
                                         Symbol("<float>")    -> FloatType,
                                         Symbol("<double>")   -> DoubleType,
                                         Symbol("<symbol>")   -> SymbolType,
                                         Symbol("<string>")   -> StringType,
                                         Symbol("<char>")     -> CharType,
                                         Symbol("<function>") -> FunctionType,
                                         Symbol("<set>")      -> SetType,
                                         Symbol("<vector>")   -> VectorType,
                                         Symbol("<map>")      -> MapType,
                                         Symbol("<list>")     -> SType,
                                         Symbol("<any>")      -> AnyType
                                         )
  final val reserved   = Set(Lambda, Quote, Quasiquote, Splice, SpliceSeq, SetBang, Def, MacroSym, If, DefMethod, Paste) ++ nameType.keySet

  @inline def isNumber(tpe : Byte) = (tpe == AnyType) || ((tpe & 0xc) == (0 : Byte))
  @inline def isCollection(tpe : Byte) = (tpe == AnyType) || ((tpe & 0x8) != (0 : Byte))
  @inline def typeOf(expr : Any) = expr match { // TODO: this is the wrong way to do this
      case i : Int => IntType
      case i : Float => FloatType
      case i : Double => DoubleType
      case i : Symbol => SymbolType
      case i : Ref => SymbolType
      case i : String => StringType
      case i : Char => CharType
      case i : List[_] => SType
      case i : Set[_] => SetType
      case i : Vector[_] => VectorType
      case i : Map[_, _] => MapType
      case i : LFunc => FunctionType
      case i : Function0[_] => FunctionType
      case i : Function1[_, _] => FunctionType
      case i : Function2[_, _, _] => FunctionType
      case i : Function3[_, _, _, _] => FunctionType
      case i : MM => FunctionType
      case _ => throw TypeError(s"$expr is not a known type")
    }
  @inline def sig(argTypes : Byte*) = {
    var res = 0L
    for (a <- argTypes) res = (res << 4) | a
    res
  }

  // ------------------------------------------------------------------------------------------------------------
  // Eval
  // ------------------------------------------------------------------------------------------------------------
  def countSplice(tree : Any, depth : Int = 1) : (Int, Any) = tree match {
      case (`Splice` | `SpliceSeq`) :: x :: Nil => countSplice(x, depth = depth + 1)
      case rest @ _ => depth -> rest
    }
  def splice(expr : Any, scope : Scope, depth : Int = 1) : Any = expr match {
      case `Quasiquote` :: x :: Nil => List(Quasiquote, splice(x, scope, depth = depth + 1))
      case full @ (`Splice` :: x :: Nil) => 
        val (count, tail) = countSplice(x)
        if (depth == count) eval(tail, scope)
        else full
      case full @ (`SpliceSeq` :: x :: Nil) => 
        val (count, tail) = countSplice(x)
        if (depth == count) List(Paste, eval(x, scope))
        else full
      case args : Map[_, _] => args map { case (k, v) => splice(k, scope, depth) -> splice(v, scope, depth) }
      case args : Iterable[Any] =>
        args.flatMap(ex => splice(ex, scope, depth) match { 
                       case List(Paste, x : Seq[Any]) => x
                       case x @ _ => List(x)
                     })
      case ex @ _ => ex
    }
  def eval(expr : Any, scope : Scope) : Any = expr match {
      case Ref(i, name) => scope(i)
      case name : Symbol => throw new ArgumentError(s"trying to evaluate a unreferenced symbol: $name")//scope(name)
      case args : List[Any] =>
        args match {
          // Special forms
          case `Lambda` :: (fargs : List[Any]) :: body =>
            val barr = body.toArray
            if (barr.length == 0) throw Error(s"malformed lambda with no body: (lambda $fargs $body)")
            //val symargs  = fargs.map { x => assert(x.isInstanceOf[Symbol], s"lambda argument $x is not a string!"); x.asInstanceOf[Symbol] }.toArray
            val symargs  = fargs.map { x => assert(x.isInstanceOf[Ref], s"lambda argument $x is not a string!"); x.asInstanceOf[Ref] }.toArray

            new LFunc(scope = scope, args = symargs) {
              def f0 = {
                var idx = 0
                while(idx < (barr.length-1)) {
                  eval(barr(idx), this.scope)
                  idx += 1
                }
                eval(barr(barr.length - 1), this.scope)
              }
            }
          //case `DefMethod` :: (name : Symbol) :: (args : List[Any]) :: body =>
          case `DefMethod` :: Ref(nindex, name) :: (args : List[Any]) :: body =>
            val barr = body.toArray
            if (barr.length == 0) throw Error(s"malformed method with no body: (defmethod $name $args $body)")
            //val symargs  = args.map { x => assert(x.isInstanceOf[Symbol], s"method argument $x is not a string!"); x.asInstanceOf[Symbol] }.toArray
            // val symargs  = args.map { 
            //     case r : Ref => r
            //     case s : Symbol => s
            //     case x @ _ => throw ArgumentError(s"method argument $x is not a string!")
            //   }.toArray
            // val symtypes = symargs.grouped(2).map { case Array(a, b) => a -> b }.toArray
            val symtypes = args.grouped(2).map { 
                case List(a : Ref, b : Symbol) => a -> b; 
                case x @ _ => throw ArgumentError(s"method argument/type pair [$x] is not a wellformed!") 
              }.toArray
            val types    = symtypes.map(x => nameType(x._2))
            val func = new LFunc(scope = scope, args = symtypes.map(_._1)) {
                def f0 = {
                  var idx = 0
                  while(idx < (barr.length-1)) {
                    eval(barr(idx), scope)
                    idx += 1
                  }
                  eval(barr(barr.length - 1), scope)
                }
              }
            if (!scope.isDefinedAt(name)) scope(name) = LMM(name = name, methods = Map[Long, LFunc]())
            scope(name).asInstanceOf[LMM].methods += sig(types : _*) -> func
          case `Lambda` :: rest => throw Error(s"malformed lambda expression (lambda ${rest})")
          case `Quote` :: rest :: Nil => rest
          case `Quasiquote` :: rest :: Nil => splice(rest, scope)
          case sp @ ((`Splice` | `SpliceSeq`) :: x :: Nil) => sp
          //case (`SetBang` | `Def`) :: (name : Symbol) :: value :: Nil =>
          case (`SetBang` | `Def`) :: Ref(nindex, name) :: value :: Nil =>
            if (!scope.isDefinedAt(name)) scope(name) = null // NOTE: create scope binding for recursion
            val res = eval(value, scope)
            scope(name) = res
            res
          //case `MacroSym` :: (name : Symbol) :: (margs : List[Any]) :: body =>
          case `MacroSym` :: Ref(nindex, name) :: (margs : List[Any]) :: body =>
            //val symargs = margs.map { x => assert(x.isInstanceOf[Symbol], s"macro argument $x is not a string!"); x.asInstanceOf[Symbol] }
            val symargs = margs.map { x => assert(x.isInstanceOf[Ref], s"macro argument $x is not a string!"); x.asInstanceOf[Ref].s }

            val mac = Macro({ (xs : List[Any]) =>
                              val newScope = Scope(scope, newBindings = (symargs zip xs) : _*)
                              var res : Any = 0
                              for (l <- body) res = eval(l, newScope)
                              res
                            })
            scope(name) = mac
            mac
          case `If` :: condExpr :: alt1 :: alt2 :: Nil =>
            val cond = eval(condExpr, scope).asInstanceOf[Boolean]
            if (cond) eval(alt1, scope)
            else eval(alt2, scope)
          case fn :: rest => // Function and macro call
            eval(fn, scope) match {
              case Macro(mac) => eval(mac(rest), scope)
              case func : LFunc => func(scope, rest)
              case func : MM => 
                rest match {
                  case List() => func()
                  case List(a) => func(eval(a, scope))
                  case List(a, b) => func(eval(a, scope), eval(b, scope))
                  case List(a, b, c) => func(eval(a, scope), eval(b, scope), eval(c, scope))
                }
              case func : Fn[_] => func(rest.map(e => eval(e, scope)))
              case func : F0[_] => func()
              case func : F1[_, _] => func(eval(rest.head, scope))
              case func : F2[_, _, _] => func(eval(rest.head, scope), eval(rest.tail.head, scope))
              case func : F3[_, _, _, _] => func(eval(rest.head, scope), eval(rest.tail.head, scope), eval(rest.tail.tail.head, scope))
              case func : LMM => func(scope, rest)
              case x @ _ => throw Error(s"$x [a literal value] cannot be used as a function")
            }
        }
      case map : Map[_, _] => map.map { case (k, v) => eval(k, scope) -> eval(v, scope) }
      case iter : Iterable[Any] => iter.map(v => eval(v, scope))
      case x @ _ => x
    }

  // ------------------------------------------------------------------------------------------------------------
  // Print
  // ------------------------------------------------------------------------------------------------------------
  def lispString(expr : Any) : String =
    expr match {
      case Symbol(s)          => s"'$s"
      case Ref(i, s)          => s"'$s"
      case List(xs @ _*)      => s"(${xs.map(e => lispString(e)).mkString(" ")})"
      case Vector(xs @ _*)    => s"[${xs.map(e => lispString(e)).mkString(" ")}]"
      case xs : Set[_]        => s"#{${xs.map(e => lispString(e)).mkString(" ")}}"
      case xs : Map[_, _]     => s"{${xs.map{ case (k, v) => lispString(k) + " " + lispString(v) }.mkString(" ")}}"
      case c : Char           => s"#\\$c"
      case e @ _              => e.toString
    }
}

// ------------------------------------------------------------------------------------------------------------
// Exceptions and helpers
// ------------------------------------------------------------------------------------------------------------
case class Error(e : String) extends Exception(e)
case class ParseError(e : String) extends Exception(e)
case class ArgumentError(e : String) extends Exception(e)
case class NotImplementedError(e : String) extends Exception(e)
case class BindingNotFound(e : String) extends Exception(e)
case class TypeError(e : String) extends Exception(e)

// ------------------------------------------------------------------------------------------------------------
// Scope
// ------------------------------------------------------------------------------------------------------------
case class Ref(i : Int, s : Symbol)

case class SymbolCache(var nameSet : Map[Symbol, Int] = Map[Symbol, Int](), var index : ArrayBuffer[Symbol] = ArrayBuffer[Symbol]()) {
  def insertName(name : Symbol) =
    if (nameSet isDefinedAt name) nameSet(name)
    else {
      val idx = nameSet.size
      nameSet += name -> idx
      index += name
      idx
    }
  def makeNameArray {
    index = ArrayBuffer.fill[Symbol](nameSet.size)(Lambda)
    for ((n, idx) <- nameSet) index(idx) = n
  }
  def apply(k : Symbol) = nameSet(k) //{ if (!nameSet.isDefinedAt(k)) update(k); nameSet(k) }
  def apply(i : Int) = index(i)
  def length = index.length
  def update(k : Symbol) {
    assert(!nameSet.isDefinedAt(k), s"Symbol $k is already defined")
    nameSet += (k -> index.length)
    index += k
  }
}

class Scope(var symtab : Vector[Any], val symbolCache : SymbolCache) {
  @inline def update(k : Int, v : Any) { symtab = symtab.updated(k, v) }
  @inline def update(k : Symbol, v : Any) {
    val idx = symbolCache.insertName(k)
    if (idx >= symtab.length) 
      symtab = symtab ++ (for (i <- symtab.length until idx) yield null)  ++ Array(v)
    else symtab = symtab.updated(idx, v)
  }
  @inline def apply(k : Int) : Any = symtab(k)
  @inline def apply(k : Symbol) : Any = symtab(symbolCache(k))
  @inline def isDefinedAt(k : Int) = symtab isDefinedAt k
  @inline def isDefinedAt(k : Symbol) = symtab isDefinedAt symbolCache(k)
}

object Scope {
  def apply(prev : Scope, newBindings : (Symbol, Any)*) = { // New bindings of prior symbols
    val sc = new Scope(prev.symtab, prev.symbolCache)
    for ((k, v) <- newBindings) sc(k) = v
    sc
  }
  def apply(bindings : (Symbol, Any)*) = { // denovo construction
    val sc = new Scope(Vector[Any](), new SymbolCache())
    for ((k, v) <- bindings) sc(k) = v
    sc
  }
}

// ------------------------------------------------------------------------------------------------------------
// Functions/macros
// ------------------------------------------------------------------------------------------------------------
case class Macro(fn : List[Any] => Any)

abstract class LFunc(var scope : Scope, val args : Array[Ref]) { // Lisp Functions
  @inline def f0 : Any
  @inline def apply(outerScope : Scope, rest : List[Any]) = {
    scope = Scope(scope)
    var idx = 0
    var t = rest
    while(t != Nil) {
      scope(args(idx).i) = eval(t.head, outerScope)
      idx += 1
      t = t.tail
    }
    f0
  }
}
case class LMM(val name : Symbol, var methods : Map[Long, LFunc]) { // Lisp multimethods
  @inline def apply(outerScope : Scope, args : List[Any]) = {
    val evaled         = args.map(r => eval(r, outerScope)).toArray
    val types          = evaled.map(r => typeOf(r))
    val mi             = sig(types : _*)
    val resolvedMethod = methods.getOrElse(mi, throw ArgumentError(s"No method matches prototype: (${name.name} ${types.map(t => typeName(t)).mkString(" ")})"))
    resolvedMethod.scope = Scope(resolvedMethod.scope)
    var idx = 0
    for (r <- 0 until args.length) {
      resolvedMethod.scope(resolvedMethod.args(idx).i) = evaled(idx)
      idx += 1
    }
    resolvedMethod.f0
  }
}

// Native Functions and multimethods
case class F0[O](fn : Function0[O]) extends Function0[Any] { def apply() = fn() }
case class F1[I, O](fn : Function1[I, O]) extends Function1[Any, Any] { def apply(i : Any) = fn(i.asInstanceOf[I]) }
case class F2[I1, I2, O](fn : Function2[I1, I2, O]) extends Function2[Any, Any, Any] { def apply(i1 : Any, i2 : Any) = fn(i1.asInstanceOf[I1], i2.asInstanceOf[I2]) }
case class F3[I1, I2, I3, O](fn : Function3[I1, I2, I3, O]) extends Function3[Any, Any, Any, Any] { 
  def apply(i1 : Any, i2 : Any, i3 : Any) = fn(i1.asInstanceOf[I1], i2.asInstanceOf[I2], i3.asInstanceOf[I3]) 
}
case class Fn[O](fn : Function1[List[Any], O]) extends Function1[List[Any], Any] { def apply(i : List[Any]) = fn(i) }

class TypeTrie(methods : List[(Array[Byte], AnyRef)]) {
  val trie = (0 to 4).map(x => makeTier).toArray[AnyRef]
  for ((sig, func) <- methods) update(sig, func)

  def makeTier = Array.fill[AnyRef](16)(null)
  def update(sig : Array[Byte], func : AnyRef) {
    if (sig.length == 0) trie(0) = func
    var tier = trie(sig.length).asInstanceOf[Array[AnyRef]]
    for ((a, level) <- sig.zipWithIndex) {
      if (level == sig.length - 1) tier(a) = func
      else {
        if (tier(a) == null) tier(a) = makeTier
        tier = tier(a).asInstanceOf[Array[AnyRef]]
      }
    }
  }
  def apply(sig : Array[Byte]) : AnyRef = {
    if (sig.length == 0) return trie(0)
    var tier = trie(sig.length).asInstanceOf[Array[AnyRef]]
    var i = 0
    while (i < (sig.length - 1)) {
      if (tier(sig(i)) != null) tier = tier(sig(i)).asInstanceOf[Array[AnyRef]]
      else return null
      i += 1
    }
    return tier(sig(sig.length - 1))
  }
  def getOrElse(sig : Array[Byte], alt : => AnyRef) = {
    val res = apply(sig)
    if (res == null) alt
    else res
  }
}

case class MM(methodList : List[(Array[Byte], AnyRef)]) {
  val methods = new TypeTrie(methodList)
  def apply() = {
    val fn = methods.getOrElse(Array[Byte](), throw ArgumentError(s"No method matches prototype: (<native>)"))
    fn.asInstanceOf[F0[Any]]()
  }
  def apply(a : Any) = {
    val sig = Array(typeOf(a))
    val fn = methods.getOrElse(sig, throw ArgumentError(s"No method matches prototype: (<native> ${typeName(typeOf(a))})"))
    fn.asInstanceOf[F1[Any, Any]](a)
  }
  def apply(a : Any, b : Any) = {
    val sig = Array(typeOf(a), typeOf(b))
    val fn = methods.getOrElse(sig, throw ArgumentError(s"No method matches prototype: (<native> ${typeName(typeOf(a))} ${typeName(typeOf(b))})"))
    fn.asInstanceOf[F2[Any, Any, Any]](a, b)
  }
  def apply(a : Any, b : Any, c : Any) = {
    val sig = Array(typeOf(a), typeOf(b), typeOf(c))
    val fn = methods.getOrElse(sig, throw ArgumentError(s"No method matches (<native> ${typeName(typeOf(a))} ${typeName(typeOf(b))} ${typeName(typeOf(c))})"))
    fn.asInstanceOf[F3[Any, Any, Any, Any]](a, b, c)
  }
}

// ------------------------------------------------------------------------------------------------------------
// Reader
// ------------------------------------------------------------------------------------------------------------
class Reader extends RegexParsers with PackratParsers {
  // TODO: 
  //  + missing support for string escape sequences
  val FloatWithDotMatcher  = """[-+]?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?[Ff]?""".r
  val FloatNoDotMatcher    = """[-+]?[0-9]*[0-9]+([eE][-+]?[0-9]+)?[Ff]""".r
  val readTable            = mMap[Char, Parser[Any]](
      '#'  -> ("{" ~> rep(expr) <~ "}" ^^ { _.toSet }),
      '{'  -> (rep(expr) <~ "}" ^^ { ex => mapFromList(ex) }),
      '['  -> (rep(expr) <~ "]" ^^ { _.toVector }),
      '\'' -> (expr ^^ { ex => List('quote, ex) }),
      '`'  -> (expr ^^ { ex => List('quasiquote, ex) }),
      '~'  -> (opt("@") ~ expr ^^ { 
                 case Some("@") ~ ex => List(SpliceSeq, ex)
                 case None ~ ex      => List(Splice, ex) })
    )

  override val whiteSpace = """([\s\n\r]*(?<!\\);[^\n\r$]+[\n\r\s$]*|[\s\n\r]+)""".r // TODO: doesn't handle strings containing ';'

  def double   = """[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?[dD]""".r ^^ { x => x.toDouble }
  def float    = (FloatWithDotMatcher | FloatNoDotMatcher ) ^^ { x => x.toFloat }
  def int      = """[-+]?\d+""".r ^^ { x => x.toInt }
  def uchar    = """\\(u[\da-fA-F]{4})""".r ^^ { x => Integer.parseInt(x.drop(2), 16).toChar }
  def achar    = """\\[\da-fA-F]{2}""".r ^^ { x => Integer.parseInt(x.drop(1), 16).toChar }
  def char     = """\\.""".r ^^ { x => augmentString(x)(1) }
  def string   = """(?<!\\)".*?(?<!\\)"""".r ^^ { x => x.drop(1).dropRight(1) }
  def bools    = """(?iu)(true|false)""".r ^^ { x => x.toLowerCase match { case "true" => true; case "false" => false } }
  def d_hash   = "#" ~> readTable('#')
  def d_quote  = "'" ~> readTable('\'')
  def d_tilde  = "~" ~> readTable('~')
  def d_quasi  = "`" ~> readTable('`')
  def d_at     = "@" ~> readTable('@')
  def d_ocurly = "{" ~> readTable('{')
  def d_obrac  = "[" ~> readTable('[')
  def symbol   = """[^\d(){}#'`,@~;~\[\]^\s][^\s()#'`,@~;^{}~\[\]]*""".r ^^ { x => Symbol(x) }
  def sexpr  : Parser[Any] = "(" ~> rep(expr) <~ ")"
  def expr   : Parser[Any] = (double | float | int | uchar | achar | char | string | bools | symbol | sexpr | 
                              d_hash | d_quote | d_ocurly | d_obrac | d_quasi | d_tilde)

  def apply(input : String, symbolCache : SymbolCache) : Any = apply(new java.io.StringReader(input), symbolCache)
  def apply(input : jReader, symbolCache : SymbolCache) : Any = { 
    def rewriteExpr(e : Any) : Any = e match {
        case name : Symbol => 
          if (reserved(name)) name 
          else Ref(symbolCache.insertName(name), name)
        case a : Map[_, _] => a.map { case (k, v) => rewriteExpr(k) -> rewriteExpr(v)  }
        case a : Iterable[Any] => a.map(ee => rewriteExpr(ee))
        case x @ _ => x
      }
    parse(expr, input) match {
      case Success(result, _) => rewriteExpr(result)
      case failure @ _ => throw ParseError(failure.toString)
    }
  }
}

// ------------------------------------------------------------------------------------------------------------
// REPL
// ------------------------------------------------------------------------------------------------------------
object REPL {
  def prompt = Console.BLUE + "scalisp> " + Console.YELLOW
  val console = {
    val consoleReader = new ConsoleReader(System.in, System.out, null, null)
    consoleReader setHistory (new FileHistory(new java.io.File(".scalisp-history")))
    consoleReader setHistoryEnabled true
    consoleReader setPrompt prompt
    consoleReader
  }
  def main(args : Array[String]) {
    val reader = new Reader
    val scope  = Scope(lib.DefaultEnvironment : _*)
    var x = 0

    while(true) {
      console.readLine() match {
        case l : String =>
          val start = System.nanoTime
          val res    = lispString(eval(reader(l, scope.symbolCache), scope))
          val dur   = System.nanoTime - start
          println(Console.CYAN + res)
          println(Console.RED  + f"result took ${dur.toDouble / 1e9}%7.2f seconds")
        case _ => 
          println(Console.RED + "exiting...")
          System.exit(0)
      }
    }

  }
}

