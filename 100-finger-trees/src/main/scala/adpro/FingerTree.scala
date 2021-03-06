package adpro
import java.util.NoSuchElementException

import scala.language.higherKinds

// AUTHOR1: Charlotte Amalie Qvist
// AUTHOR2: Arkadiusz Szczygiel

// The implementation is based on Section 3 of the paper.
//
// This implementation is designed to be eager, following the regular strictness
// of Scala.  However it would be an interesting exercise to extend it so that
// it is possibly lazy, like in the paper of Hinze and Paterson.  The obvious
// choice is to make values of elements stored in the queue lazy.  Then there is
// also a discussion of possible suspension of the middle element of the tree on
// page 7.

// Complete the implementation of Finger Trees below.  Incomplete
// places are marked ...
//
// I am Simulating a package with an object, because type declarations
// can only be placed in objects (so this allows me to place Digit on top).

object data {

  // The interface spec for reducible structures, plus two useful derived
  // reducers that the paper introduces (toList and toTree)

  // I changed the type of reducers to not use curried operators, but regular
  // binary operators.  This is more natural in Scala, and gives easier to read
  // syntax of expressions.  Curried style is preferred in Haskell.

  trait Reduce[F[_]] {
    def reduceR[A,B] (opr: (A,B) => B) (fa: F[A], b: B) :B
    def reduceL[A,B] (opl: (B,A) => B) (b: B, fa: F[A]) :B

    // page 3
    def toList[A] (fa: F[A]) :List[A] = reduceR((a:A, b:List[A]) => a::b)(fa, Nil:List[A])

    // page 6
    def toTree[A] (fa :F[A]) :FingerTree[A] =
      reduceR((a:A, b:FingerTree[A]) => FingerTree.addR(b,a))(fa, Empty():FingerTree[A])

  }

  // Types for Finger trees after Hinze and Pattersoni (page 4)

  type Digit[A] = List[A]

  sealed trait Node[+A] {

    // uncomment the delagation once Node.toList is implemented
    def toList :List[A] = Node.toList (this)
  }

  case class Node2[A] (l :A, r :A) extends Node[A]
  case class Node3[A] (l: A, m: A, r: A) extends Node[A]

  sealed trait FingerTree[+A] {

    // The following methods are convenience delagation so we can use
    // the operations both as methods and functions.
    // Uncomment them once you have implemented the corresponding functions.

    def addL[B >:A] (b: B) :FingerTree[B] = FingerTree.addL (b,this)
    def addR[B >:A] (b: B) :FingerTree[B] = FingerTree.addR (this,b)
    def toList :List[A] = FingerTree.toList (this)

    def headL :A = FingerTree.headL (this)
    def tailL :FingerTree[A] = FingerTree.tailL (this)
    def headR :A = FingerTree.headR (this)
    def tailR :FingerTree[A] = FingerTree.tailR (this)

    def empty :Boolean = FingerTree.viewL(this) match {
      case NilTree() => true
      case ConsL(_,_) => false
    }
    def nonEmpty :Boolean = !empty
  }

  case class Empty () extends FingerTree[Nothing] {
    //page 7
    override def empty = true
    override def nonEmpty = false
  }

  case class Single[A] (data: A) extends FingerTree[A]
  case class Deep[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) extends FingerTree[A]

  // page 6
  //
  // Types of views on trees
  // The types are provided for educational purposes.  I do not use the view
  // types in my implementation. I implement views as Scala extractors.
  // But you may want to implement views first like in the paper, and then
  // conver them to Scala extractors.

  // In the paper views are generic in the type of tree used. Here I make them
  // fixed for FingerTrees.

  sealed trait ViewL[+A]
  case class NilTree () extends ViewL[Nothing]
  case class ConsL[A] (hd: A, tl: FingerTree[A]) extends ViewL[A]
  case class ConsR[A] (tl: FingerTree[A], hd: A) extends ViewL[A]

  // Left extractors for Finger Trees (we use the same algorithm as viewL in the
  // paper). You can do this, once you implemented the views the book way.
  // Once the extractors are implemented you can pattern match on NilTree, ConsL
  // and ConsR
  //
  // See an example extractor implemented for Digit below (Digit.unapply)

  object NilTree { // we use the same extractor for both left and right views
    //def unapply[A] (t: FingerTree[A]) :Boolean = true
  }

  object ConsL {
    // def unapply[A] (t: FingerTree[A]) :Option[(A,FingerTree[A])] = ...
  }

  object ConsR {
    // def unapply[A] (t: FingerTree[A]) :Option[(FingerTree[A],A)] = ...
  }

  // several convenience operations for Digits.
  object Digit extends Reduce[Digit] { // uncomment once the interfaces are provided

    // page 3, top
    //
    def reduceR[A,Z] (opr: (A,Z) => Z) (d: Digit[A], z: Z) :Z = d.foldRight(z)(opr)
    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, d: Digit[A]) :Z = d.foldLeft(z)(opl)

    // Digit inherits toTree from Reduce[Digit] that we will also apply to other
    // lists, but this object is a convenient place to put it (even if not all
    // lists are digits)

    // This is a factory method that allows us to use Digit (...) like a
    // constructor
    def apply[A] (as: A*) : Digit[A] = List(as:_*)

    // This is an example of extractor, so that we can use Digit(...) in pattern
    // matching.  Case classes have extractors automatically, but Digit defined
    // as above is not a case class, but just a type name.
    def unapplySeq[A] (d: Digit[A]): Option[Seq[A]] = Some (d)
  }


  object Node extends Reduce[Node] {

    // page 5, top
    def reduceR[A,Z] (opr: (A,Z) => Z) (n :Node[A], z: Z) :Z = n match {
      case Node2(a, b) => opr(a, opr(b, z))
      case Node3(a, b, c) => opr(a, opr(b, opr(c, z)))
    }

    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, n :Node[A]) :Z = n match {
      case Node2(a, b) => opl(opl(z, b), a)
      case Node3(a, b, c) => opl(opl(opl(z, c), b), a)
    }
  }



  // Most of the paper's key functions are in the module below.

  object FingerTree extends Reduce[FingerTree] { // uncomment once the interface is implemented

    // page 5
    def reduceR[A,Z] (opr: (A,Z) => Z) (t: FingerTree[A], z: Z) :Z = t match {
      case x:Empty => z
      case Single(x) => opr(x, z)
      case Deep(t:Digit, m:FingerTree, h:Digit) =>
        Digit.reduceR(opr)(t, FingerTree.reduceR(Node.reduceR(opr) _)(m, Digit.reduceR(opr)(h, z)))
    }

    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, t: FingerTree[A]) :Z = t match {
      case x:Empty => z
      case Single(x) => opl(z,x)
      case Deep(pr, m, sf) =>
        Digit.reduceL(opl)(FingerTree.reduceL(Node.reduceL(opl) _)(Digit.reduceL(opl)(z, pr), m), sf)
    }

    // 5 bottom (the left triangle); Actually we could use the left
    // triangle in Scala but I am somewhat old fashioned ...

    def addL[A](a:A, ft:FingerTree[A]):FingerTree[A] = ft match {
      case x:Empty => Single(a)
      case Single(b) => Deep(Digit(b), Empty(), Digit(a))
      case Deep(pr, m, Digit(e, d, c, b)) => Deep(pr, (addL(Node3(e, d, c), m)), Digit(b, a))
      case Deep(pr, m, sf) => Deep(pr, m, sf :+ a)
    }

    def addR[A](ft:FingerTree[A], a:A):FingerTree[A] = ft match {
      case x:Empty => Single(a)
      case Single(b) => Deep(Digit(a), Empty(), Digit(b))
      case Deep(Digit(e, d, c, b), m, sf) => Deep(Digit(a,b), (addR(m, Node3(c, d, e))), sf)
      case Deep(pr, m, sf) => Deep(pr:+ a, m, sf)
    }

    // page 6
    // This is a direct translation of view to Scala. You can replace it later
    // with extractors in Scala, see above objects NilTree and ConsL (this is an
    // alternative formulation which is more idiomatic Scala, and slightly
    // better integrated into the language than the Haskell version).
    // In Haskell we need to call viewL(t) to pattern match on views.  In Scala,
    // with extractors in place, we can directly pattern match on t.
    //

    def viewL[A] (t: FingerTree[A]) :ViewL[A] = t match {
      case x:Empty => NilTree()
      case Single(x) => ConsL(x, Empty())
      case Deep(pr, m, sf) => ConsL(pr.head, deepL(pr.tail, m, sf))
    }


    // page 6
    // A smart constructor that allows pr to be empty
    def deepL[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) :FingerTree[A] = pr match {
      //pr empty
      case Nil => viewL(m) match {
        case NilTree() => Digit.toTree(sf)
        case ConsL(a, m) => Deep(Node.toList(a), m, sf)
      }
      case _ => Deep(pr, m, sf)
    }

    def deepR[A] (sf: Digit[A], m: FingerTree[Node[A]], pr: Digit[A]): FingerTree[A] = pr match {
      case Nil => viewL(m) match {
        case NilTree() => Digit.toTree(sf)
        case ConsR(m, a) => Deep(sf, m, Node.toList(a))
      }
      case _ => Deep(sf, m, pr)
    }

    def headL[A](ft: FingerTree[A]):A = viewL(ft) match {
      case ConsL(a, _) => a
    }
    def tailL[A] (ft: FingerTree[A]):FingerTree[A] = viewL(ft) match {
      case ConsL(_, x) => x
    }
    def headR[A](ft: FingerTree[A]):A = viewL(ft) match {
      case ConsR(_, a) => a
    }
    def tailR[A](ft: FingerTree[A]):FingerTree[A] = viewL(ft) match {
      case ConsR(x, _) => x
    }
  }

}
