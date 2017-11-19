package chapter4


object Chapter4 {

  trait Option[+A] {

    case object None extends Option[Nothing]

    case class Some[+A](get:A) extends Option[A]

    def map[B] (f : A => B) : Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f :A => Option[B]):Option[B] = this match{
      case None => None
      case Some(a) => f(a)
   }

     def getOrElse[B >: A](default: => B):B =  this match{
      case Some (a) => a
     case None => default
    }

    def orElse[B >: A] (ob: => Option[B]):Option[B]=this match{
      case Some (a) => Some(a)
      case None => ob
    }

    def filter( f:A => Boolean): Option[A] = this match{
      case Some (a) => if (f(a)) Some(a) else None
      case None => None
    }

    def map2[A,B,C](a:Option[A], b:Option[B])(f :(A,B) => C):Option[C] = (a,b) match{
      case (Some(a), Some(b)) => Some(f(a,b))
      case _ => None
    }

    def sequence[A] (a:List[Option[A]]) :Option[List[A]] = a match {
      case h :: tail => h flatMap (hh => sequence(tail) map (hh :: _))
      case Nil => None
    }
  }
}
