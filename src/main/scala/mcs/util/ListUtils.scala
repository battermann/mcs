package mcs.util

import cats.Eq
import cats.implicits._

object ListUtils {
  def isSuffixOf[A: Eq](xs: List[A], ys: List[A]): Boolean =
    if (xs.length > ys.length ) {
      false
    } else {
      ys.drop(ys.length - xs.length) === xs
    }
}
