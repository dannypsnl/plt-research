package lc.interpreter

import scala.collection.mutable.HashMap

class Env(parent: Option[Env]) {
  var internalMap = new HashMap[String, Value]()
  def get(name: String) : Value = {
    val r = internalMap.get(name)
    if (r.isEmpty && parent.isDefined) {
      parent.get.get(name)
    } else if (r.isDefined) {
      r.get
    } else {
      throw new NotFoundException(name)
    }
  }
  def set(name: String, value: Value) = {
    internalMap.put(name, value)
    ()
  }
}

class NotFoundException(name: String) extends Exception {}