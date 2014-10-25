object showScheme {

  abstract class Scheme
  case class Resistance(resistance: Int) extends Scheme
  case class Sequence(before: Scheme, after: Scheme) extends Scheme
  case class Parallel(left: Scheme, right: Scheme) extends Scheme
  type Chain = List[String]

  def width(elem: Chain) = elem(0).length

  def mshow(list: Chain): Unit = {
    val temp = list.map(x => x + "\n")
    for (el <- temp)
      print(el)
  }

  def alignHeight(elem1: Chain, elem2: Chain): (Chain, Chain) = {
    def height(elem: Chain) = elem.size

    val h1 = height(elem1)
    val h2 = height(elem2)
    var rez1 = elem1
    var rez2 = elem2
    if (h1 > h2) {
      val free: Int = (width(elem2) - 1) / 2
      for (i <- 1 to (h1 - h2)) {
        rez2 = rez2 ++ List(" " * free + "|" + " " * free)
      }
    }
    if (h1 < h2) {
      val free: Int = (width(elem1) - 1) / 2
      for (i <- 1 to (h2 - h1)) {
        rez1 = rez1 ++ List(" " * free + "|" + " " * free)
      }
    }
    (rez1, rez2)
  }


  def alignWidth(elem1: Chain, elem2: Chain): (Chain, Chain) = {
    val mwidth = math.max(width(elem1), width(elem2))
    var rez1: Chain = elem1
    var rez2: Chain = elem2

    if (mwidth == width(elem1)) {
      val dop: Int = (mwidth - 1) / 2 - (width(elem2) - 1) / 2
      rez2 = elem2.map(x => " " * dop + x + " " * dop)
    }

    if (mwidth == width(elem2)) {
      val dop: Int = (mwidth - 1) / 2 - (width(elem1) - 1) / 2
      rez1 = elem1.map(x => " " * dop + x + " " * dop)
    }
    (rez1, rez2)

  }

  def makeHeadParConn(elem1: Chain, elem2: Chain): String = {
    val lshift: Int = (width(elem1) - 1) / 2
    val rshift: Int = (width(elem2) - 1) / 2
    " " * lshift + "_" * (lshift + 1) + "|" + "_" * (rshift + 1) + " " * rshift
  }

  def makeTailParConn(elem1: Chain, elem2: Chain): List[String] = {
    val lshift: Int = (width(elem1) - 1) / 2
    val rshift: Int = (width(elem2) - 1) / 2

    List(" " * lshift + "|" + "_" * (lshift + rshift + 1) + "|" + " " * rshift) ++
      List(" " * (2 * lshift + 1) + "|" + " " * (2 * rshift + 1))
  }

  def show(scheme: Scheme): Chain = {
    scheme match {
      case Resistance(value) =>
        val dop = " " * (value.toString.length / 2) + "|" + " " * (value.toString.length / 2)
        List(dop, value.toString + (if (value.toString.length % 2 == 0) " " else ""), dop)

      case Sequence(before, after) =>
        val rez = alignWidth(show(before), show(after))
        rez._1 ++ rez._2

      case Parallel(before, after) =>
        val pairAlignWidth = alignWidth(show(before), show(after))
        val pairAlign = alignHeight(pairAlignWidth._1, pairAlignWidth._2)
        var rez: Chain = List(makeHeadParConn(pairAlign._1, pairAlign._2))

        for (i <- 0 to (pairAlign._1.size - 2))
          rez = rez ++ List(pairAlign._1(i) + " " + pairAlign._2(i))

        rez ++ makeTailParConn(pairAlign._1, pairAlign._2)
    }
  }


  val checkString =
    Parallel(
      Resistance(5622222),
      Parallel(
        Sequence(
          Resistance(45),
          Parallel(
            Resistance(34),
            Resistance(56743)
          )
        ),
        Parallel(
          Sequence(
            Resistance(4095),
            Parallel(
              Resistance(34),
              Resistance(543)
            )
          ),
          Resistance(0)
        )
      )
    )
  mshow(show(checkString))
}























