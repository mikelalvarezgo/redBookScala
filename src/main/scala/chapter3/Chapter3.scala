package chapter3


object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A])

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




}
