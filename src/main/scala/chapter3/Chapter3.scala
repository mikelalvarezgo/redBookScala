package chapter3

import scala.annotation.tailrec


object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  // 3.2
  def tail[A](list:List[A]):List[A] = list match {
    case Cons(x:A, tail:List[A]) => tail
    case Nil => Nil
  }

  // 3.3

  def setHead[A](head:A, list: List[A]) = list match  {
    case list:List[A] => Cons(head,list)
    case Nil => Cons(head,Nil)
  }

  // 3.4

  def drop[A](n: Int, list: List[A]):List[A] = n match {
    case 0 => list
    case 1 => list match  {
      case Cons(head:A,tail:List[A]) => tail
      case Nil => Nil
    }
    case _ =>list match  {
      case Cons(head:A,tail:List[A]) => drop(n-1,tail)
      case Nil => Nil
    }
  }

  // 3.4

  def dropWhile[A](list: List[A], f: A => Boolean):List[A] = list match {
    case Cons(head:A,tail:List[A]) => if (f(head)) dropWhile(list,f) else list
    case Nil => Nil
    }


  // 3.5

  def init[A](l: List[A]):List[A] = {
    l match {
      case Cons(head:A, tail:List[A]) => Cons(head,init(tail))
      case Cons(head:A, Nil) => Nil
      case Nil => sys.error("Error, init of nil")
    }
  }

  //3.7

  def product(list:List[Double]):Double = list match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // foldRigth

  def foldRight[A,B] (l: List[A], z:B)(f:(A,B) => B):B = {
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs,z)(f))
    }
  }


  // 3.8

  def length[A](list:List[A]):Int ={
    foldRight(list,0)((l,b) => b+1)
  }


  @tailrec
  def foldLeft[A,B] (l: List[A], z:B)(f:(B,A) => B):B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
    }
  }

  // 3.12

  def reverse[A](list: List[A]): List[A] = {
   foldLeft(list, List[A]())((x:List[A], xs:A) => Cons(xs, x) )
  }


  // 3.16

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  // 3.17

  def toStringL(l: List[Double]): List[String] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h.toString, t))

  // 3.18

  def map[A,B](l: List[A])(f:A =>B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))


  // 3.19

  def filter[A,B](l: List[A])(f:A =>Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) =>
      if(f(h)) Cons(h,t)
      else Cons(Nil[A],t))

  def concat[A](l: List[List[A]]): List[A] =
      foldRight(l, Nil:List[A])((a,b) => (a::b))

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // 3.23
  def zipList[A](list1:List[A], list2:List[A])(f:((A,A) =>A)):List[A] = (list1, list2) match{
    case (Nil,_) => Nil
    case (_, Nil) => Nil
    case(Cons(a,ta), Cons(b,tb)) => Cons(f(a,b),zipList(ta,tb)(f))}

  def zipListTR[A](list1:List[A], list2:List[A])(f:((A,A) =>A)):List[A] = (list1, list2) match{
    case (Nil,_) => Nil
    case (_, Nil) => Nil
    case(Cons(a,ta), Cons(b,tb)) => Cons(f(a,b),zipList(ta,tb)(f))}



  //Trees

  sealed trait Tree[+A]

  case class Branch[A](right: Tree[A], left: Tree[A]) extends Tree[A]

  case class Leaf[A](value:A) extends Tree[A]

  def size[A](tree:Tree[A]):Int =tree match{
    case Leaf(x) => 1
    case Branch(r,l) => 1 + size(r)+ size(l)
  }

  def depth[A](tree:Tree[A],level:Int = 0):Int =tree match{
    case Leaf(x) => level
    case Branch(r,l) => math.max(depth(r,level+1), depth(l,level+1))
  }

  def map[A,B](tree:Tree[A])(f: A =>B):Tree[B] =tree match{
    case Leaf(x) => Leaf(f(x))
    case Branch(r,l) => Branch(map(r)(f), map(l)(f))
  }

   def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
   case Leaf(a) => f(a)
   case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
   }

  def foldInfered[A,B](tree:Tree[A](f:A => B)(g:(B,B) => B):B =tree match{
    case Leaf(x) => f(x)
    case Branch(r,l) => g(foldInfered(r)(f)(g), foldInfered(l)(f)(g))
  }


}
