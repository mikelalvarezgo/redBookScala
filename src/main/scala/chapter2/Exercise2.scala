package chapter2


object Exercise2 extends App{

  def isSortered[A](as: Array[A], ordered: (A,A) => Boolean):Boolean = {

    def step(n:Int):Boolean = {
      if(n == as.size-1) {
        ordered(as(n-1), as(n))
      }else {
        if (ordered(as(n-2), as(n-1)))
          step(n +1)
        else false
      }
    }
    step(1)
  }

}
