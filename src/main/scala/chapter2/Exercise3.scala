package chapter2

/**
  * Created by mikelalvarezgo on 5/11/17.
  */
object Exercise3 {

  def curry[A,B,C](f :(A,B) => C): A => (B => C)= a=>b=> f(a,b)

  def uncurry[A,B,C](f :A => B => C): (A,B) => C = (a,b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a =>
    f(g(a))
}
