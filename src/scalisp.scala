package scalisp

import util.parsing.combinator.{RegexParsers, PackratParsers}
import collection.mutable.{Map => mMap, HashMap, ArrayBuffer}
import java.io.{Reader => jReader}
import tools.jline.console.ConsoleReader
import tools.jline.console.history.FileHistory
import language.dynamics
import annotation.{tailrec, switch}

object `package` {
  def mapFromList(list : List[Any]) = Map(list.grouped(2).map { case List(a, b) => a -> b }.toList : _*)
  def debug(s : String) = System.err.println(Console.RED + s"DEBUG: $s")

  final val Lambda     = Sym("lambda")
  final val Quote      = Sym("quote")
  final val Quasiquote = Sym("quasiquote")
  final val Splice     = Sym("splice")
  final val SpliceSeq  = Sym("spliceseq")
  final val SetBang    = Sym("set!")
  final val Def        = Sym("def")
  final val MacroSym   = Sym("macro")
  final val If         = Sym("if")
  final val DefMethod  = Sym("defmethod")
  final val Paste      = Sym("paste")

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
  final val nameType = Map[Sym, Byte](Sym("<short>")    -> ShortType,
                                      Sym("<int>")      -> IntType,
                                      Sym("<float>")    -> FloatType,
                                      Sym("<double>")   -> DoubleType,
                                      Sym("<symbol>")   -> SymbolType,
                                      Sym("<string>")   -> StringType,
                                      Sym("<char>")     -> CharType,
                                      Sym("<function>") -> FunctionType,
                                      Sym("<set>")      -> SetType,
                                      Sym("<vector>")   -> VectorType,
                                      Sym("<map>")      -> MapType,
                                      Sym("<list>")     -> SType,
                                      Sym("<any>")      -> AnyType
    )
  final val reserved   = Set(Lambda, Quote, Quasiquote, Splice, SpliceSeq, SetBang, Def, MacroSym, If, DefMethod, Paste) ++ nameType.keySet

  @inline def isNumber(tpe : Byte) = (tpe == AnyType) || ((tpe & 0xc) == (0 : Byte))
  @inline def isCollection(tpe : Byte) = (tpe == AnyType) || ((tpe & 0x8) != (0 : Byte))
  @inline def typeOf(expr : Any) = expr match { // TODO: this is the wrong way to do this
      case i : Int => IntType
      case i : Float => FloatType
      case i : Double => DoubleType
      case i : Sym => SymbolType
      case i : String => StringType
      case i : Char => CharType
      case i : List[_] => SType
      case i : Array[_] => SType
      case i : Set[_] => SetType
      case i : Vector[_] => VectorType
      case i : Map[_, _] => MapType
      case i : LFunc => FunctionType
      case i : Function0[_] => FunctionType
      case i : Function1[_, _] => FunctionType
      case i : Function2[_, _, _] => FunctionType
      case i : Function3[_, _, _, _] => FunctionType
      case i : MM => FunctionType
      case i : LMM => FunctionType
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
      case Array((`Splice` | `SpliceSeq`), x) => countSplice(x, depth = depth + 1)
      case rest @ _ => depth -> rest
    }
  def splice(expr : Any, scope : Scope, depth : Int = 1) : Any = expr match {
      case sf : Array[Any] => sf match {
        case Array(`Quasiquote`, x) => Array(Quasiquote, splice(x, scope, depth = depth + 1))
        case full @ Array(`Splice`, x) =>
          val (count, tail) = countSplice(x)
          if (depth == count) eval(tail, scope)
          else full
        case full @ Array(`SpliceSeq`, x) =>
          val (count, tail) = countSplice(x)
          if (depth == count) Array(Paste, eval(x, scope))
          else full
        case _ => sf.flatMap(ex => splice(ex, scope, depth) match { 
                       case Array(Paste, x : Seq[Any]) => x
                       case Array(Paste, x : Array[Any]) => x
                       case x @ _ => Array(x)
                     }) //Array(k) ++ x.map(ex => splice(ex, scope, depth))
      }
      case args : Map[_, _] => args map { case (k, v) => splice(k, scope, depth) -> splice(v, scope, depth) }
      case args : Iterable[Any] =>
        args.flatMap(ex => splice(ex, scope, depth) match { 
                       case Array(Paste, x : Seq[Any]) => x
                       case Array(Paste, x : Array[Any]) => x
                       case x @ _ => Array(x)
                     })
      case ex @ _ => ex
    }

  val dispatch = {
    val dispatchTable = Map(Lambda.index     -> lambda _,
                            Quote.index      -> quote _,
                            Quasiquote.index -> quasiquote _,
                            Splice.index     -> evalSplice _,
                            SpliceSeq.index  -> evalSplice _,
                            SetBang.index    -> setdef _,
                            Def.index        -> setdef _,
                            MacroSym.index   -> defmacro _,
                            If.index         -> ifelse _,
                            DefMethod.index  -> defmethod _
      )
    val res = Array.fill[Function3[Array[Any], Scope, Sym, Any]](reserved.size)(null)
    for ((i, v) <- dispatchTable) res(i) = v
    res
  }

  @inline def lambda(args : Array[Any], scope : Scope, self : Sym = null) = {
    val fargs = args(1).asInstanceOf[Array[Any]].map(_.asInstanceOf[Sym])
    val body  = args.drop(2)
    if (body.length == 0) throw Error(s"malformed lambda with no body: (lambda $fargs $body)")

    new LFunc(scope = Scope(scope), argNames = fargs) {
      val newsyms = if (self != null) argNames ++ Array(self) else argNames
      var i = 0
      while (i < newsyms.length) {
        scope(newsyms(i)) = if (i == argNames.length) this else null
        i += 1
      }

      @inline def f0 = {
        var idx = 0
        while(idx < (body.length-1)) {
          eval(body(idx), this.scope)
          idx += 1
        }
        eval(body(body.length - 1), this.scope)
      }
    }
  }
  @inline def defmethod(definition : Array[Any], scope : Scope, self : Sym) = {
    val slf  = definition(1).asInstanceOf[Sym]
    val args = definition(2).asInstanceOf[Array[Any]].map(_.asInstanceOf[Sym])
    val body = definition.drop(3)
    if (body.length == 0) throw Error(s"malformed method with no body: (defmethod ${slf.name} $args $body)")

    val symtypes = args.grouped(2).toArray
    val types    = symtypes.map(x => nameType(x(1)))

    if (!scope.isDefinedAt(slf)) scope(slf) = LMM(name = slf, methods = Map[Long, LFunc]())

    val func = new LFunc(scope = scope, argNames = symtypes.map(_(0))) {
        val newsyms = argNames
        var i = 0
        while (i < newsyms.length) {
          scope(newsyms(i)) = if (i == argNames.length) this else null
          i += 1
        }

        @inline def f0 = {
          var idx = 0
          while(idx < (body.length-1)) {
            eval(body(idx), this.scope)
            idx += 1
          }
          eval(body(body.length - 1), this.scope)
        }
      }
    scope(slf).asInstanceOf[LMM].methods += sig(types : _*) -> func
  }
  @inline def quote(expr : Array[Any], scope : Scope, self : Sym) = expr(1)
  @inline def quasiquote(expr : Array[Any], scope : Scope, self : Sym) = splice(expr(1), scope)
  @inline def evalSplice(expr : Array[Any], scope : Scope, self : Sym) = expr
  @inline def setdef(args : Array[Any], scope : Scope, self : Sym) = {
    val slf = args(1).asInstanceOf[Sym]
    val value = args(2)
    val res = eval(value, scope, self = slf)
    scope(slf) = res
    res
  }
  @inline def defmacro(exprs : Array[Any], scope : Scope, self : Sym) = {
    val slf   = exprs(1).asInstanceOf[Sym]
    val margs = exprs(2).asInstanceOf[Array[Any]].map(_.asInstanceOf[Sym])
    val body  = exprs.drop(3)

    val mac = Macro({ (xs : Array[Any]) =>
                      val newScope = Scope(scope)
                      var res : Any = 0
                      for (i <- 0 until margs.length) newScope(margs(i)) = xs(i)
                      for (l <- body) res = eval(l, newScope)
                      res
                    })
    scope(slf) = mac
    mac
  }
  @inline def ifelse(exprs : Array[Any], scope : Scope, self : Sym) = {
    if (eval(exprs(1), scope).asInstanceOf[Boolean]) eval(exprs(2), scope)
    else if (exprs.length == 4) eval(exprs(3), scope)
    else Unit
  }
  @inline def funcall(function : Any, args : Array[Any], scope : Scope) = function match {
      case func : LFunc => func(scope, args)
      case func : MM =>
        (args.length : @switch) match {
          case 1 => func()
          case 2 => func(eval(args(1), scope))
          case 3 => func(eval(args(1), scope), eval(args(2), scope))
          case 4 => func(eval(args(1), scope), eval(args(2), scope), eval(args(3), scope))
        }
      case func : Fn[Any] => func(args.drop(1).map(e => eval(e, scope)))
      case func : F0n[_] => func()
      case func : F1n[Any, Any] => func(eval(args(1), scope))
      case func : F2n[Any, Any, Any] => func(eval(args(1), scope), eval(args(2), scope))
      case func : F3n[Any, Any, Any, Any] => func(eval(args(1), scope), eval(args(2), scope), eval(args(3), scope))
      case func : LMM => func(scope, args)
      case mac : Macro => eval(mac(args.drop(1)), scope)
      case x @ _ => throw Error(s"$x [a literal value] cannot be used as a function")
    }
  def eval(expr : Any, scope : Scope, self : Sym = null) : Any = expr match {
      case name : Sym => scope(name.index)
      case num : Int => num
      case num : Float => num
      case num : Double => num
      case char : Char => char
      case string : String => string
      case list : Array[Any] =>
        list(0) match {
          case s : Sym if (s.index < dispatch.length && dispatch(s.index) != null) => dispatch(s.index)(list, scope, self)
          case s : Sym => funcall(scope(s.index), list, scope)
          case _ => funcall(eval(list(0), scope), list, scope)
        }
      case map : Map[_, _] => map.map { case (k, v) => eval(k, scope) -> eval(v, scope) }
      case iter : Iterable[Any] => iter.map(v => eval(v, scope))
      case _ => expr
    }

  // ------------------------------------------------------------------------------------------------------------
  // Print
  // ------------------------------------------------------------------------------------------------------------
  @inline def lispString(expr : Any) : String =
    expr match {
      case Sym(s, i)          => s"'$s"
      case List(xs @ _*)      => s"(${xs.map(e => lispString(e)).mkString(" ")})"
      case Array(xs @ _*)     => s"(${xs.map(e => lispString(e)).mkString(" ")})"
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
case class SyntaxError(e : String) extends Exception(e)

// ------------------------------------------------------------------------------------------------------------
// Scope
// ------------------------------------------------------------------------------------------------------------
class Scope(var symtab : Array[Any]) {
  @inline def update(k : Int, v : Any) { symtab(k) = v }
  @inline def update(k : Sym, v : Any) {
    val idx = k.index
    if (idx >= symtab.length)
      symtab = symtab ++ (for (i <- symtab.length until idx) yield null)  ++ Array(v)
    else symtab(idx) = v
  }
  @inline def apply(k : Int) : Any = symtab(k)
  @inline def apply(k : Sym) : Any = symtab(k.index)
  @inline def isDefinedAt(k : Int) : Boolean = k < symtab.length && symtab(k) != null
  @inline def isDefinedAt(k : Sym) : Boolean = isDefinedAt(k.index)
}

object Scope {
  @inline def apply(prev : Scope) = new Scope(prev.symtab.clone)
  @inline def apply(bindings : (Sym, Any)*) = {
    val sc = new Scope(Array[Any]())
    for ((k, v) <- bindings) sc(k) = v
    sc
  }
}

// ------------------------------------------------------------------------------------------------------------
// Functions/macros
// ------------------------------------------------------------------------------------------------------------
case class Macro(fn : Array[Any] => Any) { @inline def apply(a : Array[Any]) = fn(a) }

abstract class LFunc(var scope : Scope, val argNames : Array[Sym]) { // Lisp Functions
  @inline def f0 : Any
  @inline def apply(outerScope : Scope, rest : Array[Any]) = {
    scope = Scope(scope)
    var idx = 1
    while (idx < rest.length) {
      scope(argNames(idx-1).index) = eval(rest(idx), outerScope)
      idx += 1
    }
    f0
  }
}
case class LMM(val name : Sym, var methods : Map[Long, LFunc]) { // Lisp multimethods
  var tmpSpace = Array.fill[Any](10)(null)
  @inline def apply(outerScope : Scope, args : Array[Any]) = {
    if (args.length > tmpSpace.length) tmpSpace = Array.fill[Any](tmpSpace.length + 5)(null)
    var idx = 1
    var mi  = 0L
    while (idx < args.length) {
      tmpSpace(idx - 1) = eval(args(idx), outerScope)
      mi = (mi << 4) | typeOf(tmpSpace(idx - 1))
      idx += 1
    }

    val resolvedMethod   = 
      methods.getOrElse(mi,
                        throw ArgumentError(s"No method matches prototype: (${name.name} ${tmpSpace.slice(0, args.length-1).map(t => typeName(typeOf(t))).mkString(" ")})"))
    resolvedMethod.scope = Scope(resolvedMethod.scope)

    idx = 0
    while (idx < resolvedMethod.argNames.length) {
      resolvedMethod.scope(resolvedMethod.argNames(idx).index) = tmpSpace(idx)
      idx += 1
    }
    resolvedMethod.f0
  }
}

abstract class F0n[O] extends Function0[O] { @inline def apply() : O }
abstract class F1n[I, O] extends Function1[I, O] { @inline def apply(i : I) : O }
abstract class F2n[I1, I2, O] extends Function2[I1, I2, O] { @inline def apply(i1 : I1, i2 : I2) : O  }
abstract class F3n[I1, I2, I3, O] extends Function3[I1, I2, I3, O] { @inline def apply(i1 : I1, i2 : I2, i3 : I3) : O }
abstract class Fn[O] extends Function1[Array[Any], O] { @inline def apply(i : Array[Any]) : O }

class TypeTrie(methods : List[(Array[Byte], AnyRef)]) {
  val trie = (0 to 4).map(x => makeTier).toArray[AnyRef]
  for ((sig, func) <- methods) update(sig, func)

  def makeTier = Array.fill[AnyRef](16)(null)
  def update(sig : Array[Byte], func : AnyRef) {
    if (sig.length == 0) trie(0) = func
    var tier = trie(sig.length).asInstanceOf[Array[AnyRef]]
    var level = 0
    while (level < sig.length) {
      if (level == sig.length - 1) tier(sig(level)) = func
      else {
        if (tier(sig(level)) == null) tier(sig(level)) = makeTier
        tier = tier(sig(level)).asInstanceOf[Array[AnyRef]]
      }
      level += 1
    }
  }
  @inline def apply(sig : Array[Byte]) : AnyRef = {
    if (sig.length == 0) return trie(0)
    var tier = trie(sig.length).asInstanceOf[Array[AnyRef]]
    var i = 0
    while (i < (sig.length - 1)) {
      if (tier(sig(i)) != null) tier = tier(sig(i)).asInstanceOf[Array[AnyRef]]
      else throw ArgumentError(s"No method matches prototype: (<native> ${sig.map(a => typeName(a)).mkString(" ")})") //return null
      i += 1
    }
    if (tier(sig(sig.length - 1)) == null) throw ArgumentError(s"No method matches prototype: (<native> ${sig.map(a => typeName(a)).mkString(" ")})")
    tier(sig(sig.length - 1))
  }
}

case class MM(methodList : List[(Array[Byte], AnyRef)]) {
  val tmpSpace0 = Array[Byte]()
  val tmpSpace1 = Array[Byte](0)
  val tmpSpace2 = Array[Byte](0, 0)
  val tmpSpace3 = Array[Byte](0, 0, 0)
  val tmpSpace4 = Array[Byte](0, 0, 0, 0)
  val methods = new TypeTrie(methodList)

  def apply() = methods(tmpSpace0).asInstanceOf[F0n[Any]]()
  def apply(a : Any) = {
    tmpSpace1(0) = typeOf(a)
    methods(tmpSpace1).asInstanceOf[F1n[Any, Any]](a)
  }
  def apply(a : Any, b : Any) = {
    tmpSpace2(0) = typeOf(a)
    tmpSpace2(1) = typeOf(b)
    methods(tmpSpace2).asInstanceOf[F2n[Any, Any, Any]](a, b)
  }
  def apply(a : Any, b : Any, c : Any) = {
    tmpSpace3(0) = typeOf(a)
    tmpSpace3(1) = typeOf(b)
    tmpSpace3(2) = typeOf(c)
    methods(tmpSpace3).asInstanceOf[F3n[Any, Any, Any, Any]](a, b, c)
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
      '\'' -> (expr ^^ { ex => Array(Quote, ex) }),
      '`'  -> (expr ^^ { ex => Array(Quasiquote, ex) }),
      '~'  -> (opt("@") ~ expr ^^ { 
                 case Some("@") ~ ex => Array(SpliceSeq, ex)
                 case None ~ ex      => Array(Splice, ex) })
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
  def symbol   = """[^\d(){}#'`,@~;~\[\]^\s][^\s()#'`,@~;^{}~\[\]]*""".r ^^ { x => Sym(x) }
  def sexpr  : Parser[Any] = "(" ~> rep(expr) <~ ")" ^^ { _.toArray } 
  def expr   : Parser[Any] = (double | float | int | uchar | achar | char | string | bools | symbol | sexpr | 
                              d_hash | d_quote | d_ocurly | d_obrac | d_quasi | d_tilde)

  def apply(input : String) : Any = apply(new java.io.StringReader(input))
  def apply(input : jReader) : Any = { 
    parse(expr, input) match {
      case Success(result, _) => result
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
          val res    = lispString(eval(reader(l), scope))
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

