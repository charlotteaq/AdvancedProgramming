package Curry

/**
  * Created by charlotteqvist on 10/05/2016.
  */
class curry {

  def curry[A,B,C](f: (A, B) => C): A =>(B =>C) = a => b => f(a, b)

}
