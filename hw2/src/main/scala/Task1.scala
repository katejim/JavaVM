/**
 * Created by kate on 02.10.14.
 */
object Task1 extends App {

  def computeY(p: BigInt, x: BigInt): BigInt = x * (20 * p + x)

  def computeX(p: BigInt, c: BigInt): Int =
    (0 to 9).reverse.find(x => computeY(p, x) <= c) head

  def computeP(pPrev: BigInt, xPrev: Int): BigInt = pPrev * 10 + xPrev

  def computeC(cPrev: BigInt, yPrev: BigInt): BigInt = (cPrev - yPrev) * 100

  def computeOneNum(inX: List[Int], c: BigInt, p: BigInt):
  (List[Int], BigInt, BigInt) = {
    val x: Int = computeX(p, c)
    (x :: inX, computeC(c, computeY(p, x)), computeP(p, x))
  }

  def result(precision: Int): List[Int] = {
    val s: Stream[(List[Int], BigInt, BigInt)] =
      Stream.iterate((Nil: List[Int], BigInt(2), BigInt(0)))((x: (List[Int], BigInt, BigInt)) => computeOneNum(x._1, x._2, x._3))
    s.take(precision).map(_._1).last.reverse
  }

  print(result(32))

}
