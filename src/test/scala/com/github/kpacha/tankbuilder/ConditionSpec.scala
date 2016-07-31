package com.github.kpacha.tankbuilder

import org.scalatest._

class ConditionSpec extends FlatSpec with Matchers with TankBiulderSuite {

  "An ArithmeticCondition" should "marshall into a simple XML element" in {
    arCond.toXML.toString should be("<phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase>")
  }

  it should "unmarshall a simple XML element" in {
    Condition.phraseFromXML(arCond.toXML).toString should be(arCond.toString)
  }

  it should "unmarshall all possible operations" in {
    Condition.typesOfArithOps map (Condition.ArithmeticCondition(constant, _, input)) foreach { c =>
      Condition.phraseFromXML(c.toXML).toString should be(c.toString)
    }
  }

  "A BooleanCondition" should "marshall into a simple XML element" in {
    boolCond.toXML.toString should be("<phrase type=\"boolean\" op=\"||\"><left><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase></left><right><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase></right></phrase>")
  }

  it should "unmarshall a simple XML element" in {
    Condition.phraseFromXML(boolCond.toXML).toString should be(boolCond.toString)
  }

  it should "unmarshall all possible operations" in {
    Condition.typesOfBooleanOps map (Condition.BooleanCondition(arCond, _, arCond)) foreach { c =>
      Condition.phraseFromXML(c.toXML).toString should be(c.toString)
    }
  }

  "A Condition" should "marshall into a simple XML element (1)" in {
    cond1.toXML.toString should be("<condition isLoop=\"true\"><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase><statement><action type=\"someAction\"><expression type=\"constant\">42.0</expression></action></statement></condition>")
  }

  it should "marshall into a simple XML element (2)" in {
    cond2.toXML.toString should be("<condition isLoop=\"false\"><phrase type=\"boolean\" op=\"||\"><left><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase></left><right><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase></right></phrase><statement><condition isLoop=\"true\"><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase><statement><action type=\"someAction\"><expression type=\"constant\">42.0</expression></action></statement></condition></statement></condition>")
  }

  it should "unmarshall a simple XML element (1)" in {
    Condition.fromXML(cond1.toXML).toString should be(cond1.toString)
  }

  it should "unmarshall a simple XML element (2)" in {
    Condition.fromXML(cond2.toXML).toString should be(cond2.toString)
  }

}