import org.scalatest.*
import flatspec.*
import matchers.*

import nop.frontend.support.*
import scala.collection.mutable.HashMap
import scala.collection.mutable.AbstractMap

class ScopedMapTests extends AnyFlatSpec with should.Matchers {
  // TODO: give okay names and/or comments for private helpers.
  private def idPair(l: Int, r: Int): IndexedSeq[(Int, Int)] = l to r map { i => i -> i }

  private def mapCheck[K, V](left: AbstractMap[K, V], right: AbstractMap[K, V]): Unit = {
    for (k, v) <- left do {
      right(k) should be(v)
    }
  }

  private def mapCheckNot[K, V](left: AbstractMap[K, V], right: AbstractMap[K, V]): Unit = {
    for (k, _) <- left do {
      right.contains(k) should be(false)
    }

  }

  "ScopedMap" should "be a correct map instance" in {
    val inners       = idPair(0, 10)
    val innersSecond = idPair(11, 20)

    val scmap = ScopedMap.from(inners)
    val map   = HashMap.from(inners)

    val secondMap = HashMap.from(innersSecond)

    mapCheck(scmap, map)

    mapCheck(map, scmap)

    scmap.addScope(HashMap.from(innersSecond))

    mapCheck(secondMap, scmap)

    mapCheck(map, scmap)

    scmap.leaveScope()

    mapCheck(map, scmap)

    mapCheckNot(secondMap, scmap)

  }

  // TODO: generalize test
  "ScopedMap" should "correctly enter and leave scopes." in {
    val firstScope  = idPair(0, 10)
    val secondScope = idPair(11, 20)
    val thirdScope  = idPair(21, 30)
    val fourthScope = idPair(31, 40)
    val fifthScope  = idPair(41, 50)

    val firstMap  = HashMap.from(firstScope)
    val secondMap = HashMap.from(secondScope)
    val thirdMap  = HashMap.from(thirdScope)
    val fourthMap = HashMap.from(fourthScope)
    val fifthMap  = HashMap.from(fifthScope)

    val scmap = ScopedMap.from(firstScope)

    mapCheck(scmap, firstMap)
    mapCheck(firstMap, scmap)

    scmap.addScope(HashMap.from(secondScope))

    mapCheck(firstMap, scmap)
    mapCheck(secondMap, scmap)

    scmap.addScope(HashMap.from(thirdScope))

    mapCheck(firstMap, scmap)
    mapCheck(secondMap, scmap)
    mapCheck(thirdMap, scmap)

    scmap.addScope(HashMap.from(fourthScope))

    mapCheck(firstMap, scmap)
    mapCheck(secondMap, scmap)
    mapCheck(thirdMap, scmap)
    mapCheck(fourthMap, scmap)

    scmap.addScope(HashMap.from(fifthScope))

    mapCheck(firstMap, scmap)
    mapCheck(secondMap, scmap)
    mapCheck(thirdMap, scmap)
    mapCheck(fourthMap, scmap)
    mapCheck(fifthMap, scmap)

    scmap.leaveScope()

    mapCheck(firstMap, scmap)
    mapCheck(secondMap, scmap)
    mapCheck(thirdMap, scmap)
    mapCheck(fourthMap, scmap)
    mapCheckNot(fifthMap, scmap)

    scmap.leaveScope()

    mapCheck(firstMap, scmap)
    mapCheck(secondMap, scmap)
    mapCheck(thirdMap, scmap)
    mapCheckNot(fourthMap, scmap)
    mapCheckNot(fifthMap, scmap)

    scmap.leaveScope()

    mapCheck(firstMap, scmap)
    mapCheck(secondMap, scmap)
    mapCheckNot(thirdMap, scmap)
    mapCheckNot(fourthMap, scmap)
    mapCheckNot(fifthMap, scmap)

    scmap.leaveScope()

    mapCheck(firstMap, scmap)
    mapCheckNot(secondMap, scmap)
    mapCheckNot(thirdMap, scmap)
    mapCheckNot(fourthMap, scmap)
    mapCheckNot(fifthMap, scmap)

    scmap.leaveScope()

    mapCheckNot(firstMap, scmap)
    mapCheckNot(secondMap, scmap)
    mapCheckNot(thirdMap, scmap)
    mapCheckNot(fourthMap, scmap)
    mapCheckNot(fifthMap, scmap)
  }
}
