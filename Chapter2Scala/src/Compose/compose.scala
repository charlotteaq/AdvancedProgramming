package Compose

/**
  * Created by charlotteqvist on 10/05/2016.
  */
class compose {

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}
