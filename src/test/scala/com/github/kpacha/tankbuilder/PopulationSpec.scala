package com.github.kpacha.tankbuilder

import org.scalatest._

class PopulationSpec extends FlatSpec with Matchers with TankBiulderSuite {
  val xmlVersion = population.toXML

  population.saveAsXML("src/test/resources/current_generation.xml")
  val parsedPop = Population.loadXMLFile("src/test/resources/current_generation.xml")

  "A Population" should "marshall into a simple XML element" in {
    xmlVersion.toString should be("""<population generation="1"><individual generation="1" id="1" fitness="0"><method name="run"><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition><condition isLoop="false"><phrase type="boolean" op="||"><left><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></left><right><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></right></phrase><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition></statement></condition><action type="someAction"><expression type="constant">42.0</expression></action></statement></method></individual>""" +
      """<individual generation="1" id="2" fitness="0"><method name="onHitByBullet"><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition><condition isLoop="false"><phrase type="boolean" op="||"><left><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></left><right><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase></right></phrase><statement><condition isLoop="true"><phrase type="arithmetic" op="!="><left><expression type="constant">42.0</expression></left><right><expression type="input">something</expression></right></phrase><statement><action type="someAction"><expression type="constant">42.0</expression></action></statement></condition></statement></condition><action type="someAction"><expression type="constant">42.0</expression></action></statement></method></individual></population>""")
  }

  it should "unmarshall a simple XML element" in {
    val parsedPop = Population.fromXML(xmlVersion)
    parsedPop.individuals.size should be(population.individuals.size)
    parsedPop.individuals zip population.individuals foreach (p => p._1.toString should be(p._2.toString))
  }

  it should "unmarshall a XML file (1)" in {
    parsedPop.individuals.size should be(2)
    (0 to 1) foreach { i =>
      parsedPop.individuals(i).id should be(population.individuals(i).id)
      parsedPop.individuals(i).generation should be(population.individuals(i).generation)
      parsedPop.individuals(i).fitness should be(population.individuals(i).fitness)
    }
  }

  it should "unmarshall a XML file (2)" in {
    parsedPop.individuals(0).toString should be(population.individuals(0).toString)
  }

  it should "unmarshall a XML file (3)" in {
    parsedPop.individuals(1).toString should be(population.individuals(1).toString)
  }
}