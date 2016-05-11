// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// An ADT of Lists

object Test extends App{

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  println(x)
}

//Exercise 1 - What will be the result of the following match expression?
//The result of the following match expression is 3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = as match {
    case Cons(x, xs) => xs
    case _ => Nil
  }

  // Exercise 3

  def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
    case Cons(x, xs) => Cons(newHead, xs)
    case _ => Nil
  }

  // Exercise 4

  def drop[A] (l: List[A], n: Int) : List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if(n > 0) drop(xs, n-1) else xs
  }

  // Exercise 5

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => Nil
  }

  // Exercise 6

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // Exercise 7 is in the bottom of the file

  // Exercise 8

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

  def length[A] (as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 9

  @annotation.tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 10

  def sum (as : List[Int]) : Int = foldLeft(as, 0)((item, acc) => item+acc)
  def product (as :List[Int]) : Int = foldLeft(as, 1) ((item, acc) => item * acc)
  def length1 (as :List[Int]) : Int = foldLeft(as, 0)((item, acc) => acc + 1)

  // Exercise 11

  def reverse[A] (as :List[A]) :List[A] = foldLeft(as, List[A] ())((acc, x) => Cons(x, acc))

  // Exercise 12

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = foldLeft(as, z)((b: B, a: A) => f(a, b))

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = foldRight(as, z)((a: A, b: B) => f(b, a))

  // Exercise 13

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]) :List[A] = foldRight(as, Nil: List[A])(append)

  // Exercise 14

  def map[A,B] (a :List[A]) (f :A => B) :List[B] = foldRight(a, Nil: List[B])((el: A, acc: List[B]) => Cons(f(el), acc))

  // Exercise 15 (no coding)
  //In the lecture we have presented a version of map implemented using foldRight. Why haven’t we used foldLeft instead?
  //If we were using foldLeft instead of foldRight the result would have been reversed.

  // Exercise 16

  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = foldRight(as, Nil: List[A])((el: A, acc: List[A]) =>
    if (f(el)) Cons(el, acc)
    else acc
  )

  // Exercise 17

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = concat(map(as)(f))

  // Exercise 18

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = flatMap(l)((a: A) =>
    if (p(a)) List(a)
    else Nil
  )

  // Exercise 19

  def add (l: List[Int]) (r: List[Int]): List[Int] = l match {
    case Cons(l, ls) => r match {
      case Cons(r, rs) => Cons(l + r, add(ls)(rs))
      case _ => Nil
    }
    case _ => Nil
  }

  // Exercise 20

  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = l match {
    case Cons(l, ls) => r match {
      case Cons(r, rs) => Cons(f(l, r), zipWith(f)(ls, rs))
      case _ => Nil
    }
    case _ => Nil
  }

  // Exercise 21

  @annotation.tailrec
  def startsWith[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = sup match {
    case Nil => false
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }

  // Exercise 22

  // def pascal (n :Int) : List[Int] = ...
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))

}

// Exercise 7

object Exercise7 {

  case class SalaryLine(name: String, amount: Integer)

  def maximumSalary (salaries: List[SalaryLine]) :Integer = salaries match {
    case Cons(x, Nil) => x.amount
    case Cons(x, xs) => if (x.amount > maximumSalary(xs)) x.amount
    else maximumSalary(List.drop(salaries, 0))
    case Nil => -1
  }
  val test_case = List( SalaryLine("John",41),
    SalaryLine("Alice", 42),
    SalaryLine("Bob",40))

  val test_case1 = List(SalaryLine("John",1),
    SalaryLine("Alice", 2),
    SalaryLine("Bob",3))

  val test_case2 = List(SalaryLine("John",3),
    SalaryLine("Alice", 2),
    SalaryLine("Bob",1))

  val test_case3 = List(SalaryLine("John",1))

  val test_case4 = List(SalaryLine("",0))

}


