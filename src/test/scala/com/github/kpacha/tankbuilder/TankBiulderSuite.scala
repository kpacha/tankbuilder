package com.github.kpacha.tankbuilder

trait TankBiulderSuite {
  lazy val constant = Expression.Constant(42)
  lazy val input = Expression.Input("something")
  lazy val node = Expression.ExpressionNode(constant, Expression.ExpressionOp("+"), input)

  lazy val arCond = Condition.ArithmeticCondition(constant, "!=", input)
  lazy val boolCond = Condition.BooleanCondition(arCond, "||", arCond)

  lazy val action = Action("someAction", constant)

  lazy val cond1 = Condition(true, arCond, new Statement(List(action)))
  lazy val cond2 = Condition(false, boolCond, new Statement(List(cond1)))

  lazy val stmt = new Statement(List(cond1, cond2, action))

  lazy val individual1 = new Individual(1, 1, Map("run" -> stmt))
  lazy val individual2 = new Individual(1, 2, Map("onHitByBullet" -> stmt))

  lazy val population = new Population(1, List(individual1, individual2))
}