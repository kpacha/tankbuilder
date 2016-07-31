package com.github.kpacha.tankbuilder

import org.scalatest._

class ExpressionSpec extends FlatSpec with Matchers with TankBiulderSuite {

  "A Constant Expression" should "marshall into a simple XML element" in {
    constant.toXML.toString should be("<expression type=\"constant\">42.0</expression>")
  }

  it should "unmarshall a simple XML element" in {
    Expression.fromXML(constant.toXML) should be(constant)
  }

  "An Input Expression" should "marshall into a simple XML element" in {
    input.toXML.toString should be("<expression type=\"input\">something</expression>")
  }

  it should "unmarshall a simple XML element" in {
    Expression.fromXML(input.toXML) should be(input)
  }

  "An Expression Node" should "marshall into a simple XML element" in {
    node.toXML.toString should be("<expression type=\"node\" op=\"+\"><left><expression type=\"constant\">42.0</expression></left><right><expression type=\"input\">something</expression></right></expression>")
  }

  it should "unmarshall a simple XML element" in {
    Expression.fromXML(node.toXML).toString should be(node.toString)
  }

}