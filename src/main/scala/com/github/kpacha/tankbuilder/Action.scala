package com.github.kpacha.tankbuilder

import scala.xml.{Elem, Node}
import Expression.ExpressionTree

object Action {
  def random = Action(randomType, Expression.random)
  def apply(actionType: String, tree: ExpressionTree): Action = new Action(actionType, tree)
  val types = Seq("setAhead", "setBack", "setFire", "setTurnLeftRadians", "setTurnRadarLeftRadians", "setTurnGunLeftRadians", "setTurnRightRadians",
    "setTurnRadarRightRadians", "setTurnGunRightRadians", "ahead", "back", "fire", "turnLeftRadians", "turnRadarLeftRadians", "turnGunLeftRadians",
    "turnRightRadians", "turnRadarRightRadians", "turnGunRightRadians")
  def apply(tree: ExpressionTree): Action = Action(randomType, tree)
  def apply(): Action = Action(randomType, Expression.random)
  def randomType = types(Random.generator.nextInt(types.size))

  def fromXML(node: Node) = Action((node \@ "type"), ((node \ "expression") map Expression.fromXML).head)
}

class Action(actionType: String, tree: ExpressionTree) extends StatementPart {
  import Statement._

  override def toString = actionType + "(" + tree.toString + ");\n";
  def mutate = if (Random.generator.nextBoolean) Action(actionType, tree.mutate) else Action(Action.randomType, tree)

  def mixWith(that: StatementPart) = that match {
    case s: Statement => that mixWith this
    case _ => new Statement(List(this, that))
  }

  def fromXML(node: Node): Action = Action fromXML node
  def toXML(): Elem = <action type={actionType}>{ tree.toXML }</action>
}