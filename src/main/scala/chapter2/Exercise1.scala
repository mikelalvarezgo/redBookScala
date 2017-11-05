package chapter2


object Exercise1 extends App{

  def fibonnacci(n:Int):Int = {
    def step(n: Int,prev: Int, next: Int):Int = n match{
      case 0 => prev
      case 1 => next
      case _ => step(n -1, next, prev + next)
    }
    step(n, 0, 1)
  }

  println("[FIBONACCI] Introduce number for calculate fibonacci sequence")
  val n = scala.io.StdIn.readInt()
  println(s"[FIBONACCI] Fibonacci number of $n  is ${fibonnacci(n)}")
}
