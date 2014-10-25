//////
def gcd(a: Int, b: Int): Int = {
  if (b == 0) a
  else gcd(b, a % b)
}
gcd(12, 13)
///////
def fib(a: Int): Int = {
  if (a == 0) 1
  else if (a == 1) 1
  else fib(a - 1) + fib(a - 2)
}
fib(12)

//////
def qsort(inData: List[Int]): List[Int] = {
  if (inData.length < 2)
    inData
  else {
    val p = inData(0)
    qsort(inData filter (_ < p)) ++
      (inData filter (_ == p)) ++
      qsort(inData filter (_ > p))
  }
}
var inList = List(6, 2, 3, 4, 6, 6, 9, 0)
println(qsort(inList))


