package nop
package frontend
package support

import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.AbstractSet

/** Stack of hashmaps. Useful for collecting scope information with shadowing. */
class ScopedSet[K] private (repr: Stack[HashSet[K]]) extends AbstractSet[K] {

  override def contains(key: K): Boolean = repr.find(_.contains(key)).isDefined

  override def subtractOne(elem: K): this.type = {
    repr.top.subtractOne(elem)
    this
  }

  override def addOne(elem: K): this.type = {
    repr.top.addOne(elem)
    this
  }

  def iterator: Iterator[K] = repr.iterator.flatten

  def clear(): Unit = repr.clear()

  /** ScopedMap specific methods. */
  def enterScope(): Unit                = addScope(HashSet())
  def leaveScope(): Unit                = repr.pop()
  def addScope(scope: HashSet[K]): Unit = repr.push(scope)
}

object ScopedSet {

  def empty[K]: ScopedSet[K] = ScopedSet()

  def from[K](it: IterableOnce[K]): ScopedSet[K] = ScopedSet(HashSet.from(it))

  /** Factory methods */
  def apply[K](): ScopedSet[K]    = new ScopedSet(Stack())
  def apply[K](scope: HashSet[K]) = new ScopedSet(Stack(scope))
}
