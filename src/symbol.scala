package scalisp
import collection.mutable.ArrayBuffer

final class Sym private (val name : String, val index : Int) extends Serializable {
  @inline override def toString() = s"<$name:$index>"
  @inline override def hashCode = name.hashCode()
  @inline override def equals(other : Any) = if (other.isInstanceOf[Sym]) index == other.asInstanceOf[Sym].index else false //this eq other.asInstanceOf[AnyRef]
}

object Sym {
  import java.util.HashMap
  val index  = new HashMap[String, Sym]
  val revidx = ArrayBuffer[Sym]()
  var idx    = 0

  def apply(name : String) = {
    if (index containsKey name) index.get(name)
    else {
      val res = new Sym(name, idx)
      index.put(name, res)
      revidx += res
      idx += 1
      res
    }
  }
  def unapply(sym : Sym) : Option[(String, Int)] = Some(sym.name, sym.index)
}
