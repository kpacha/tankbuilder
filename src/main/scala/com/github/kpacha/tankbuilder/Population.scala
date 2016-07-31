package com.github.kpacha.tankbuilder

import scala.xml.{ Elem, Node, NodeSeq }

object Population {
  val eliteSize = .1
  def random(size: Integer) = new Population(0, (for (id <- 0 until size) yield Individual.random(id)).toList)
  def loadXMLFile(file: String) = fromXML(scala.xml.XML.loadFile(file))
  def fromXML(node: Node) = {
    val individuals = (node \ "individual").toList map (Individual fromXML _)
    new Population((node \@ "generation").toInt, individuals.sortBy(_.id))
  }
}

class Population(generation: Integer, val individuals: List[Individual]) {
  lazy val byFitness = individuals.sortBy(-_.fitness)
  lazy val viables = individuals.filter(_.fitness > 0)
  val tournmentSize = individuals.size / 10

  def fittest: Individual = byFitness.head
  def getIndividuals = individuals.toArray

  def torunementWinner(size: Integer) = tournement(size).sortBy(-_.fitness).head
  def tournement(size: Integer) = Random.shuffle(viables).take(size)

  def nextGeneration = {
    val eliteSize = (Population.eliteSize * individuals.size).toInt
    val elite = (for (id <- 0 until eliteSize) yield new Individual(generation + 1, id, byFitness(id).methods)).toList
    val offspring = (for (id <- eliteSize until individuals.size) yield (torunementWinner(tournmentSize) reproduceWith (torunementWinner(tournmentSize), id))).toList
    new Population(generation + 1, elite ::: offspring)
  }

  def toXML: Elem = <population generation={ generation.toString }>{ individuals map (_.toXML) }</population>
}