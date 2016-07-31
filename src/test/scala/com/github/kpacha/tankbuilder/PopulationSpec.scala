package com.github.kpacha.tankbuilder

import org.scalatest._

class PopulationSpec extends FlatSpec with Matchers with TankBiulderSuite {
  val xmlVersion = population.toXML

  "A Population" should "marshall into a simple XML element" in {
    xmlVersion.toString should be("""<population generation="1"><individual generation="1" id="1" fitness="0"><method name="public void run() {""" + "\n\tsetAdjustGunForRobotTurn(true);\n\tsetAdjustRadarForGunTurn(true);\n\tsetAdjustRadarForRobotTurn(true);\n" +
      """"><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition><condition isLoop="false"><phrase type="boolean" op="||"><left><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></left><right><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></right></phrase><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition></statement></condition><action type="someAction"><expression type="constant">42.0</expression></action></statement></method></individual>"""+
      """<individual generation="1" id="2" fitness="0"><method name="public void onHitByBullet(HitByBulletEvent event) {""" + "\n" +
      """"><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition><condition isLoop="false"><phrase type="boolean" op="||"><left><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></left><right><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></right></phrase><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition></statement></condition><action type="someAction"><expression type="constant">42.0</expression></action></statement></method></individual></population>""")
  }

  it should "unmarshall a simple XML element" in {
    val parsedPop = Population.fromXML(xmlVersion)
    parsedPop.individuals.size should be(population.individuals.size)
    parsedPop.individuals zip population.individuals foreach(p => p._1.toString should be(p._2.toString))
  }
}