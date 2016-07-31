package com.github.kpacha.tankbuilder

import org.scalatest._

class ActionSpec extends FlatSpec with Matchers with TankBiulderSuite {

  "An Action" should "marshall into a simple XML element" in {
    action.toXML.toString should be("<action type=\"someAction\"><expression type=\"constant\">42.0</expression></action>")
  }

  it should "unmarshall a simple XML element" in {
    Action.fromXML(action.toXML).toString should be(action.toString)
  }

}