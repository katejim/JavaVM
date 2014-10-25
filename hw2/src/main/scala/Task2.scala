/**
 * Created by kate on 02.10.14.
 */
object Task2 extends App {
  val triangle: Stream[List[Int]] = List(1) #:: triangle.map(helper)

  def helper(inList: List[Int]): List[Int] =
    1 :: (inList.zip(inList.tail).map(x => x._1 + x._2) :+ 1)

  triangle take 2 foreach println
}

