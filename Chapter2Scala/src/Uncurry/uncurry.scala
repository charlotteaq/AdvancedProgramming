package Uncurry

/**
  * Created by charlotteqvist on 10/05/2016.
  */
class uncurry {

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a, b) => f(a)(b)

}
