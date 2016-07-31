package com.github.kpacha.tankbuilder

import org.scalatest._

class StatementSpec extends FlatSpec with Matchers with TankBiulderSuite {
  val xmlVersion = stmt.toXML

  "A Statement" should "marshall into a simple XML element" in {
    xmlVersion.toString should be("<statement><condition isLoop=\"true\"><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase><statement><action type=\"someAction\"><expression type=\"constant\">42.0</expression></action></statement></condition><condition isLoop=\"false\"><phrase type=\"boolean\" op=\"||\"><left><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase></left><right><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase></right></phrase><statement><condition isLoop=\"true\"><phrase type=\"arithmetic\" op=\"!=\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></phrase><statement><action type=\"someAction\"><expression type=\"constant\">42.0</expression></action></statement></condition></statement></condition><action type=\"someAction\"><expression type=\"constant\">42.0</expression></action></statement>")
  }

  it should "unmarshall a simple XML element" in {
    Statement.fromXML(xmlVersion).toString should be(stmt.toString)
  }

}