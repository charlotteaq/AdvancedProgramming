package isSorted

/**
  * Created by charlotteqvist on 10/05/2016.
  */
class isSorted {

  /** Check isSorted, which checks whether an Array[A] is sorted according to a given comparison function. **/

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
      @annotation.tailrec
      def iter(i: Int): Boolean = {
        if(i >= as.length - 1) true
        else !gt(as(i), as(i + 1)) && iter(i + 1)
      }
    iter(0)
  }

}
