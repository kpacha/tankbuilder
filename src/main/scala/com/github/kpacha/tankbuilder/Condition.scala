package com.github.kpacha.tankbuilder

import scala.xml.{ Elem, Node }
import Expression.ExpressionTree

object Condition {
  def apply(isLoop: Boolean, phrase: ConditionPhrase, statement: StatementPart) = new Condition(isLoop, phrase, statement)
  def random: Condition = Condition(Random.generator.nextBoolean && Random.generator.nextBoolean && Random.generator.nextBoolean, randomArithOperation, Statement.random)
  val typesOfArithOps = Seq("<", "<=", "==", "!=", ">=", ">")
  val typesOfBooleanOps = Seq("&&", "||")
  def nextArithOperation = typesOfArithOps(Random.generator.nextInt(typesOfArithOps.size))
  def randomArithOperation = ArithmeticCondition(Expression.random, nextArithOperation, Expression.random)
  def nextBooleanOperation = typesOfBooleanOps(Random.generator.nextInt(typesOfBooleanOps.size))

  def phraseFromXML(node: Node): ConditionPhrase = (node \@ "type") match {
    case "arithmetic" =>
      ArithmeticCondition(((node \ "left" \ "expression") map Expression.fromXML).head, (node \@ "op"), ((node \ "right" \ "expression") map Expression.fromXML).head)
    case "boolean" =>
      BooleanCondition(((node \ "left" \ "phrase").toList map phraseFromXML).head, (node \@ "op"), ((node \ "left" \ "phrase").toList map phraseFromXML).head)
  }

  def fromXML(node: Node): Condition = {
    val conditionType = (node \@ "isLoop") == "true"
    Condition(conditionType, ((node \ "phrase").toList map phraseFromXML).head, Statement fromXML ((node \ "statement").toList map (_.child)).flatten)
  }

  sealed trait ConditionPhrase {
    val changeLeft = 0.33
    val changeOp = 0.66

    override def toString = this match {
      case ArithmeticCondition(l, op, r) => "(" + l + op + r + ")"
      case BooleanCondition(l, op, r) => "(" + l + op + r + ")"
    }
    def mutate: ConditionPhrase = this match {
      case ArithmeticCondition(l, op, r) => {
        if (Random.generator.nextBoolean) BooleanCondition(this, nextBooleanOperation, Condition.randomArithOperation)
        else {
          val rnd = Random.generator.nextDouble
          if (rnd < changeLeft) ArithmeticCondition(l.mutate, op, r)
          else if (rnd < changeOp) ArithmeticCondition(l, nextArithOperation, r)
          else ArithmeticCondition(l, op, r.mutate)
        }
      }
      case BooleanCondition(l, op, r) => {
        val rnd = Random.generator.nextDouble
        if (rnd < changeLeft) BooleanCondition(l.mutate, op, r)
        else if (rnd < changeOp) BooleanCondition(l, nextBooleanOperation, r)
        else BooleanCondition(l, op, r.mutate)
      }
    }

    def toXML(): Elem = this match {
      case ArithmeticCondition(l, op, r) => <phrase type="arithmetic" op={ op.toString }><left>{ l.toXML }</left><right>{ r.toXML }</right></phrase>
      case BooleanCondition(l, op, r) => <phrase type="boolean" op={ op.toString }><left>{ l.toXML }</left><right>{ r.toXML }</right></phrase>
    }
  }

  case class ArithmeticCondition(left: ExpressionTree, operation: String, right: ExpressionTree) extends ConditionPhrase
  case class BooleanCondition(left: ConditionPhrase, operation: String, right: ConditionPhrase) extends ConditionPhrase
}

class Condition(isLoop: Boolean, phrase: Condition.ConditionPhrase, statement: StatementPart) extends StatementPart {
  val changeIsLoop = 0.2
  val changePhrase = 0.6

  override def toString = (if (isLoop) "while(" else "if(") + phrase + "){\n" + statement + "}\n"
  def mutate = {
    val rnd = Random.generator.nextDouble
    if (rnd < changeIsLoop) Condition(!isLoop, phrase, statement)
    else if (rnd < changePhrase) Condition(isLoop, phrase.mutate, statement)
    else Condition(isLoop, phrase, statement.mutate)
  }

  def mixWith(that: StatementPart) = that match {
    case s: Statement => that mixWith this
    case _ => new Statement(List(this, that))
  }

  def fromXML(node: Node): Condition = Condition fromXML node
  def toXML(): Elem = <condition isLoop={ isLoop.toString }>{ phrase.toXML }{ statement.toXML }</condition>
}