package nop
package frontend
package support

import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import scala.collection.mutable.AbstractMap
import scala.collection.MapFactory
import scala.collection.mutable

/** Stack of hashmaps. Useful for collecting scope information with shadowing. */
class ScopedMap[K, V] private (repr: Stack[HashMap[K, V]]) extends AbstractMap[K, V] {

  override def get(key: K): Option[V] = {
    repr.collectFirst {
      case map if map.contains(key) => map.get(key).get
    }
  }

  override def iterator: Iterator[(K, V)] = repr.iterator.flatten

  override def addOne(elem: (K, V)): this.type = {
    repr.top.addOne(elem)
    this
  }

  override def subtractOne(elem: K): this.type = {
    repr.top.subtractOne(elem)
    this
  }

  /** ScopedMap specific methods. */
  def enterScope(): Unit                   = addScope(HashMap())
  def leaveScope(): Unit                   = repr.pop()
  def addScope(scope: HashMap[K, V]): Unit = repr.push(scope)

}

object ScopedMap extends MapFactory[ScopedMap] {

  def empty[K, V]: ScopedMap[K, V] = ScopedMap()

  def from[K, V](it: IterableOnce[(K, V)]): ScopedMap[K, V] = ScopedMap(HashMap.from(it))

  /** newBuilder does not work :) it just explodes. If there will be any ideas in the future as to
    * how to write that, will come back to it.
    */
  def newBuilder[K, V]: mutable.Builder[(K, V), ScopedMap[K, V]] = ???

  /** Factory methods */
  def apply[K, V](): ScopedMap[K, V]    = new ScopedMap(Stack())
  def apply[K, V](scope: HashMap[K, V]) = new ScopedMap(Stack(scope))
}
