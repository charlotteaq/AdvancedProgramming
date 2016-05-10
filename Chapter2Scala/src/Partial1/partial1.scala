package Partial1

/**
  * Created by charlotteqvist on 10/05/2016.
  */
class partial1 {

  def partial1 [A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)

}
