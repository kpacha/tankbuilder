package com.github.kpacha.tankbuilder

import scala.xml.{ Elem, Node }

object Expression {
  private val inputTypes = Seq("getX()", "getY()", "getTime()", "getHeadingRadians()", "getRadarHeadingRadians()", "getRadarTurnRemainingRadians()", "getGunHeat()",
    "getGunHeadingRadians()", "getGunTurnRemainingRadians()", "getEnergy()", "getDistanceRemaining()", "getTurnRemainingRadians()", "event.getHeadingRadians()",
    "event.getEnergy()", "event.getDistance()", "event.getVelocity()", "event.getBearingRadians()", "event.getPower()")

  def random: ExpressionTree = if (Random.generator.nextBoolean) randomConstant else randomInput

  def randomConstant = Constant(Random.generator.nextDouble * 2000 - 100)
  def randomInput = Input(inputTypes(Random.generator.nextInt(inputTypes.size)))

  def fromXML(node: Node): ExpressionTree = (node \@ "type") match {
    case "constant" => Constant(node.text.toDouble)
    case "input" => Input(node.text)
    case "node" =>
      val l = ((node \ "left") flatMap (l => (l \ "expression") map fromXML)).head
      val r = ((node \ "right") flatMap (l => (l \ "expression") map fromXML)).head
      ExpressionNode(l, ExpressionOp((node \@ "op")), r)
  }

  object ExpressionOp {
    private val types = Seq("+", "-", "*", "/", "%")
    def apply(value: String) = new ExpressionOp(value)
    def random = new ExpressionOp(types(Random.generator.nextInt(types.size)))
  }

  class ExpressionOp(value: String) {
    override def toString = value
    def mutate = ExpressionOp.random
  }

  trait ExpressionTree {
    override def toString = this match {
      case Constant(c) => c.toString
      case Input(i) => i
      case ExpressionNode(l, op, r) => l.toString + op.toString + r.toString
    }

    def mutate: ExpressionTree = this match {
      case Constant(c) =>
        if (Random.generator.nextBoolean) Constant(c * (2 * Random.generator.nextDouble - 1))
        else if (Random.generator.nextBoolean) ExpressionNode(this, ExpressionOp.random, Expression.random)
        else randomInput
      case Input(i) =>
        if (Random.generator.nextBoolean) randomInput
        else if (Random.generator.nextBoolean) ExpressionNode(this, ExpressionOp.random, Expression.random)
        else randomConstant
      case ExpressionNode(l, op, r) =>
        val rnd = 3 * Random.generator.nextDouble
        if (rnd < 1) ExpressionNode(l.mutate, op, r)
        else if (rnd < 2) ExpressionNode(l, op.mutate, r)
        else ExpressionNode(l, op, r.mutate)
    }
    def toXML(): Elem = this match {
      case Constant(c) => <expression type="constant">{ c }</expression>
      case Input(i) => <expression type="input">{ i }</expression>
      case ExpressionNode(l, op, r) => <expression type="node" op={ op.toString }><left>{ l.toXML }</left><right>{ r.toXML }</right></expression>
    }
  }

  case class Constant(value: Double) extends ExpressionTree
  case class Input(value: String) extends ExpressionTree
  case class ExpressionNode(left: ExpressionTree, operation: ExpressionOp, right: ExpressionTree) extends ExpressionTree
}