package com.github.kpacha.tankbuilder

import org.scalatest._

class IndividualSpec extends FlatSpec with Matchers with TankBiulderSuite {
  val xmlVersion = individual1.toXML

  "An Individual" should "marshall into a simple XML element" in {
    xmlVersion.toString should be("""<individual generation="1" id="1" fitness="0"><method name="public void run() {""" + "\n\tsetAdjustGunForRobotTurn(true);\n\tsetAdjustRadarForGunTurn(true);\n\tsetAdjustRadarForRobotTurn(true);\n" +
      """"><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition><condition isLoop="false"><phrase type="boolean" op="||"><left><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></left><right><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></right></phrase><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition></statement></condition><action type="someAction"><expression type="constant">42.0</expression></action></statement></method></individual>""")
  }

  it should "unmarshall a simple XML element" in {
    Individual.fromXML(xmlVersion).toString should be(individual1.toString)
  }
}