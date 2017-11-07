package chapter3


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






}
