package com.github.kpacha.tankbuilder

import org.scalatest._

class IndividualSpec extends FlatSpec with Matchers with TankBiulderSuite {
  val xmlVersion1 = individual1.toXML
  val xmlVersion2 = individual2.toXML

  "An Individual" should "marshall into a simple XML element (1)" in {
    xmlVersion1.toString should be("""<individual generation="1" id="1" fitness="0"><method name="run"><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition><condition isLoop="false"><phrase type="boolean" op="||"><left><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></left><right><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></right></phrase><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition></statement></condition><action type="someAction"><expression type="constant">42.0</expression></action></statement></method></individual>""")
  }

  it should "marshall into a simple XML element (2)" in {
    xmlVersion2.toString should be("""<individual generation="1" id="2" fitness="0"><method name="onHitByBullet"><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition><condition isLoop="false"><phrase type="boolean" op="||"><left><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></left><right><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></right></phrase><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition></statement></condition><action type="someAction"><expression type="constant">42.0</expression></action></statement></method></individual>""")
  }

  it should "unmarshall a simple XML element (1)" in {
    Individual.fromXML(xmlVersion1).toString should be(individual1.toString)
  }

  it should "unmarshall a simple XML element (2)" in {
    Individual.fromXML(xmlVersion2).toString should be(individual2.toString)
  }
}