object MyModule {

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int):Int =
      if(n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0 , 1)
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if(n >= as.length - 1) true
      else if(gt(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  private def formatFib(n : Int) = {
    val msg = "The fib of %d is %d."
    msg.format(n, fib(n))
  }


  private def isSorted(arrayToSort: Array[Int], sortingFunction: (Int, Int) => Boolean) = {
    val msg = "The supplied array is sorted %b."
    msg.format(isSorted[Int](arrayToSort, sortingFunction))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatFib(8))
    println(isSorted(Array(4,3,2), (a: Int, b: Int) => a <= b))
}
