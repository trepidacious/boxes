package boxes.util

import collection.mutable.{WeakHashMap, MultiMap, Set}

class WeakMultiMap[A, B] extends WeakHashMap[A, Set[B]] with MultiMap[A, B] {

  //Use weak sets so we retain neither keys nor values
  protected override def makeSet: Set[B] = new WeakHashSet[B]

}