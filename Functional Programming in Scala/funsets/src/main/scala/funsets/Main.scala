package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  def set(elements: List[Int]): Set = {
    def iter(elements: List[Int], e: Int): Boolean =
      if (elements.isEmpty) false else if (elements.head == e) true else iter(elements.tail, e)
    (e => iter(elements, e))
  }

  def unionSet: Set = union(singletonSet(1), singletonSet(2))
  printSet(unionSet)

  printSet(set(List(1, 2, 3, 4)))

  printSet(diff(set(List(1, 2, 3, 4, 5)), set(List(1, 2))))

  println(forall(set(List(1, 2, 3, 4, 5)), (x => x < 6)))

  println(exists(set(List(1, 2, 3, 4, 5)), (x => x == 4)))

  println(exists(set(List(1, 2, 3, 4, 5)), (x => x == 6)))

  printSet(map(set(List(1, 2, 3, 4, 5)), (x => x * 2)))

  printSet(filter(set(List(1, 2, 3, 4, 5)), (x => x > 3)))
}
