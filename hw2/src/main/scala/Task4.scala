import scala.collection.immutable.IndexedSeq

/**
 * Created by kate on 04.10.14.
 */
object Task4 extends App {

  var dictGLOBAL: Map[(Int, Int), Set[Int]] = Map()

  def divide(inBorder: (Int, Int)): IndexedSeq[((Int, Int), (Int, Int))] =
    (inBorder._1 to (inBorder._2 - 1)).map(x => ((inBorder._1, x), ((x + 1), inBorder._2)))

  def connectSets(set1: Set[Int], set2: Set[Int]): Set[Int] =
    (for (x <- set1; y <- set2) yield x * y) ++
      (for (x <- set1; y <- set2) yield x + y) ++
      (for (x <- set1; y <- set2) yield x - y)


  def solver(range: (Int, Int)): Unit = {
    if (!(dictGLOBAL contains range)) {
      if (range._2 - range._1 < 1) {
        dictGLOBAL += (range -> Set(range._1))
      } else {
        var partitions = divide(range)
        var listOfSubintervals: List[Set[Int]] = List()
        for ((left, right) <- partitions) {
          solver(left)
          solver(right)
          listOfSubintervals = listOfSubintervals :+ connectSets(dictGLOBAL(left), dictGLOBAL(right))
        }
        dictGLOBAL += (range -> listOfSubintervals.reduceRight(_ ++ _))
      }
    }
  }


  def result(num: Int, range: (Int, Int) = (1, 10)): String = {
    solver(range)
    if (dictGLOBAL(range).contains(num))
      "yep"
    else
      "nope"
  }


  println(result(6098, (1, 10)))

}
