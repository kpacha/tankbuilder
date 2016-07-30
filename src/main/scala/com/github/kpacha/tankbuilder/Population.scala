package com.github.kpacha.tankbuilder

object Population {
  val eliteSize = .1
  def random(size: Integer) = new Population(0, (for (id <- 0 until size) yield Individual.random(id)).toList)
}

class Population(generation: Integer, val individuals: List[Individual]) {
  lazy val byFitness = individuals.sortBy(-_.fitness)
  lazy val viables = individuals.filter(_.fitness > 0)

  def fittest: Individual = byFitness.head
  def getIndividuals = individuals.toArray

  def torunementWinner(size: Integer) = tournement(size).sortBy(-_.fitness).head
  def tournement(size: Integer) = Random.shuffle(viables).take(size)

  def nextGeneration = {
    val eliteSize = (Population.eliteSize * individuals.size).toInt
    val elite = (for (id <- 0 until eliteSize) yield new Individual(generation + 1, id, byFitness(id).methods)).toList
    val offspring = (for (id <- eliteSize until individuals.size) yield (torunementWinner(10) reproduceWith (torunementWinner(10), id))).toList
    new Population(generation + 1, elite ::: offspring)
  }
}