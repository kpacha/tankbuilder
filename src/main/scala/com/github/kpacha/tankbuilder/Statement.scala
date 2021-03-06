package com.github.kpacha.tankbuilder

import scala.xml.{ Elem, Node }

trait StatementPart {
  def toString: String
  def mutate: StatementPart
  def mixWith(that: StatementPart): StatementPart
  def fromXML(node: Node): StatementPart
  def toXML(): Node
}

object Statement {
  def randomStatementPart = if (Random.generator.nextBoolean && Random.generator.nextBoolean) Condition.random else Action.random
  def random = new Statement(List(randomStatementPart))
  def empty = new Statement(Nil)

  def fromXML(node: Node): StatementPart = node.label match {
    case "condition" => Condition fromXML node
    case "action" => Action fromXML node
    case "statement" => new Statement(node.child.toList map fromXML)
  }
}

class Statement(val parts: List[StatementPart] = Nil) extends StatementPart {
  import Statement._

  private val addRange = 0.2
  private val removeRange = addRange + 0.1
  private val shuffleRange = removeRange + 0.1

  override def toString = (parts foldLeft "")(_ + _)

  def mutate =
    if (parts.isEmpty) random
    else {
      val place = Random.generator.nextInt(parts.size)
      val (before, after) = parts.splitAt(place)
      Random.generator.nextDouble match {
        case x if x < addRange => new Statement(before ::: (randomStatementPart :: after))
        case x if x < removeRange => new Statement(before ::: after.tail)
        case x if x < shuffleRange => new Statement(Random.shuffle(parts))
        case _ => new Statement(before ::: (after.head.mutate :: after.tail))
      }
    }

  def mixWith(that: StatementPart) = that match {
    case s: Statement =>
      if (s.parts.isEmpty) this
      else if (parts.isEmpty) s
      else {
        val place = Random.generator.nextInt(math.min(parts.size, s.parts.size))
        new Statement((parts take place) ::: (s.parts drop place))
      }
    case c: Condition => new Statement(c :: parts)
    case a: Action => new Statement(a :: parts)
  }

  def fromXML(node: Node): StatementPart = Statement fromXML node
  def toXML(): Elem = <statement>{ parts map (_.toXML) }</statement>
}
