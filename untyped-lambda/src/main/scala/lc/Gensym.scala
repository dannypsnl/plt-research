package lc

import java.util.HashMap

// take from https://github.com/twitter/rsc/blob/master/rsc/src/main/scala/rsc/gensym/Gensym.scala
final class Gensym () {
  private val counters = new HashMap[String, Long]
  def apply(prefix: String): String = {
    val nextCounter = counters.get(prefix) + 1
    counters.put(prefix, nextCounter)
    prefix + nextCounter
  }
}

object Gensym {
  def apply(): Gensym = {
    new Gensym
  }
}
