package Fibonnacci

/**
  * Created by charlotteqvist on 10/05/2016.
  */
/**
  * Created by charlotteqvist on 08/05/2016.
  */
class fibonacci {

  //http://peter-braun.org/2012/06/fibonacci-numbers-in-scala/

  /** The first version uses recursion: **/

  def fib1(n: Int): Int = n match {
    case 0 | 1 => n
    case _ => fib1(n-1) + fib1(n-2)
  }

  /** Recursion only works well if n is small, otherwise you get a stack overflow exception. **/
  //One way to overcome this problem is to use a loop like this:

  def fib2(n: Int): Int = {
    var a = 0
    var b = 1
    var i = 0

    while (i < n ) {
      val c = a + b
      a = b
      b = c
      i + i + 1
    }
    return a
  }

  /** As you can see, this is not an elegant solution. **/
  //A better way is to use so-called tail-recursion:

  def fib3(n: Int): Int = {
    def fib_tail(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => fib_tail(n-1, b, a+b)
    }
    return fib_tail(n, 0, 1)
  }

  /** Now, we change the problem a little bit. **/
  /** We only want to compute the last six digits of the Fibonacci number. **/
  /** We modify our last algorithm a little bit: **/

  def fib4(n: Int): Int = {
    def fib_tail(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => fib_tail(n-1, b, (a+b)%1000000)
    }
    return fib_tail(n, 0, 1)
  }

  /** To get the last six digits, we use a modulo operation. However, if we assume n beyond 1 billion, **/
  /** even this algorithm takes to much time. On my computer it takes about 7 seconds to computer the result for n = 1,000,000,000. **/
  /** Is there any way to improve the algorithm? Well, there is and the mathematical theory for it is called Pisano period. **/
  /** The Pisano period, is the period with which the sequence of Fibonacci numbers, modulo k repeats. **/
  /** For example, the period of Fibonacci numbers modulo k = 3 has length 8. **/
  /** The Pisano periods when k = 10^m with m > 2 equals 15·10^(m−1). Thus, for k = 10^6, the periodicity is 1,500,000. **/
  /** According to this, we change our last algorithm a last time:**/

  def fib5(n: Int): Int = {
    def fib_tail(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => fib_tail(n-1, b, (a+b)%1000000)
    }
    return fib_tail(n%1500000, 0, 1)
  }

  /** By doing so, we can compute the Fibonacci number modulo 1,000,000 for n = 1 billion is about 10 milliseconds. **/
}

