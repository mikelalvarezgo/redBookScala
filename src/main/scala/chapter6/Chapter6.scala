package chapter6

import sun.java2d.SurfaceDataProxy.CountdownTracker

import scala.annotation.tailrec

/**
  * Created by mikelalvarezgo on 25/7/18.
  */
object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt = (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    ((if (i < 0) -(i + Int.MaxValue) else i), r)
  }

  // Exercise 2

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    ((i / Int.MaxValue).toDouble, r)
  }

  // Exercise 3

  def intDouble(rNG: RNG): ((Int, Double), RNG) = {
    val (i, r) = rNG.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)

  }

  def doubleInt(rNG: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rNG)
    val (i, r2) = r.nextInt
    ((d, i), r2)

  }

  def double3(rNG: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rNG)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Exercise 6.4

  def ints(count: Int)(rNG: RNG): (List[Int], RNG) = {
    @tailrec
    def step(count: Int)(l: List[Int], rng: RNG): (List[Int], RNG) = count match {
      case 0 => (l, rng)
      case _ => {
        val (i, r) = rng.nextInt
        step(count - 1)(i :: l, r)
      }
    }

    step(count)(List.empty, rNG)
  }


  type Rand[+A] = RNG => (A, RNG)

  val int:Rand[Int] = _.nextInt

  def unit[A](a:A):Rand[A] = rng => (a, rng)


  def map[A,B](s:Rand[A])(f: A => B):Rand[B] = rng =>{
    val (a,rng2) = s(rng)
    (f(a),rng2)
  }

  // Exercise 6.5

  def fancyToDouble = map(nonNegativeInt)(i => i.toDouble /Int.MaxValue.toDouble )

  //Exercise 6.6

  def map2[A,B,C] (ra: Rand[A], rb: Rand[B])(f: (A,B) => C):Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a,b), rng2)
  }

  // Exercise 6.7

  def sequences[A](fs:List[Rand[A]]):Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))((a,ls) => map2(a,ls)(_ :: _))

  }

  //Exercise 6.8

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]):Rand[B] = rng =>{
    val (a,rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n) - mod >= Int.MaxValue) unit(mod) else nonNegativeLessThan(n)
    }
  }

  //Exercise 6.9

  def map[A,B](s:Rand[A])(f: A => B):Rand[B] = flatMap(s){a => unit(f(a))}

  def map2[A,B,C] (ra: Rand[A], rb: Rand[B])(f: (A,B) => C):Rand[C] = flatMap(ra){a => map(rb){b => f(a,b)}}

  //Exercise 6.10

  object State {

    type State[S, +A] = S => (A,S)

    case class State[S, +A] (run: S=> (A,S))

    def map[A,B,S](s:State[S,A])(f: A => B):State[S,B] = rng =>{
      val (a,rng2) = s.run(rng)
      (f(a),rng2)
    }
    def map2[A,B,C,S] (ra: State[S,A], rb: State[S,B])(f: (A,B) => C):State[S,C] = rng => {
      val (a, rng1) = ra.run(rng)
      val (b, rng2) = rb.run(rng1)
      (f(a,b), rng2)
    }

    def flatMap[A,B,S](f: State[S,A])(g: A => State[S,B]):State[S,B] = rng =>{
      val (a,rng2) = f.run(rng)
      g(a).run(rng2)
    }
  }

  // Exercise 6.11

  sealed trait Input
  type State[S, +A] = S => (A,S)

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked:Boolean, candies:Int, coins: Int) {

    def process(input: Input):Machine = input match {
      case Coin =>
        if(locked && candies>0) copy(locked= false)
        else this
      case Turn =>
        if (!locked) copy(locked =true, coins = coins+ 1)
        else this
    }
  }

  def update1 =

  def simulateMachine(inputs: List[Input]):State[Machine,(Int,Int)] ={
    inputs.foldLeft(State(Machine))

  }

  def update2 =
    (i: Input) =>
      (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _))        => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _))  => s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + res0)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - res1, coin)
        }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)
}
}
