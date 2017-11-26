package chapter5


object chapter5 {

  sealed trait Stream[+A] 


  case object Empty extends Stream[Nothing]

  case class Cons[+A] (h:() => A, t:() => Stream[A]) extends Stream[A]
  object Stream {

    def cons[A](hd: =>A, t: => Stream[A] ):Stream[A] = {
      lazy val head = hd
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def empty[A] : Stream[A] = Empty

    def apply[A](as: A*):Stream[A] = {
      if(as.isEmpty) empty else cons(as.head,apply(as.tail: _*))
    }

    def toList[A](stream:Stream[A]):List[A]=
      stream match {
        case Empty => Nil
        case Cons(h, tail) =>
          List(h.apply()) ::: toList(tail.apply())
       }


    def drop[A](n:Int, stream: Stream[A]):Stream[A] = stream match {
      case Cons(h,tail ) if n == 0 => tail()
      case Cons(h,tail ) if n != 0 => drop(n-1, tail())
      case Empty => Empty
    }

    def forAll[A](p: A => Boolean, stream: Stream[A]):Boolean = stream match {
      case Cons(h,tail) if (p(h())) => forAll(p,tail())
      case Cons(h,tail) if !(p(h())) => false
      case Empty => true
    }
    def foldRigth[A,B](z: => B, stream: Stream[A])(d: (A, =>B)=>B):B = stream match {
      case Cons(h,t) => d(h(),foldRigth(z,t())(d))
      case _ => z
    }

    def Constant[A](a:A): Stream[A] = Cons(() =>a,() =>Constant(a))

    def from(n:Int): Stream[Int] = Cons(() =>n,() =>from(n+1))

    def fibs(n:Int) = {
      def go(f0: Int, f1: Int): Stream[Int] =
        cons(f0, go(f1, f0 + f1))
      go(1,2)
    }


  }

  class RStream[A] extends Stream[A] {


  }

}
