package com.github.kpacha.tankbuilder

object Random {
  val generator = new java.util.Random
  def shuffle[T](list: List[T]) = scala.util.Random.shuffle(list)
}