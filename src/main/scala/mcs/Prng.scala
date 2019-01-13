package mcs

import cats.data.State

object Prng {

  final case class Seed(seed: Long) {
    private lazy val next = Seed(seed * 6364136223846793005L + 1442695040888963407L)

    def nextInt: (Seed, Int) = {
      (next, (next.seed >>> 16).asInstanceOf[Int])
    }

    def nonNegativeInt: (Seed, Int) = {
      val (seed, value) = nextInt
      (seed, if (value < 0) -(value + 1) else value)
    }

    def nextInt(bound: Int): (Seed, Int) = {
      val (seed, value) = nonNegativeInt
      (seed, value % bound)
    }
  }

  def nextInt(bound: Int): State[Seed, Int] = State[Seed, Int](s => s.nextInt(bound))
}
