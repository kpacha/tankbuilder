package com.github.kpacha.tankbuilder

import scala.xml.{ Elem, Node }

object Individual {
  val methodSignature = Map(
    "onBulletHit" -> "public void onBulletHit(BulletHitEvent event) {\n",
    "onBulletHitBullet" -> "public void onBulletHitBullet(BulletHitBulletEvent event) {\n",
    "onBulletMissed" -> "public void onBulletMissed(BulletMissedEvent event) {\n",
    "onHitByBullet" -> "public void onHitByBullet(HitByBulletEvent event) {\n",
    "onHitRobot" -> "public void onHitRobot(HitRobotEvent event) {\n",
    "onHitWall" -> "public void onHitWall(HitWallEvent event) {\n",
    "onScannedRobot" -> "public void onScannedRobot(ScannedRobotEvent event) {\n",
    "run" -> "public void run() {\n\tsetAdjustGunForRobotTurn(true);\n\tsetAdjustRadarForGunTurn(true);\n\tsetAdjustRadarForRobotTurn(true);\n")

  val methods = methodSignature.keySet.toList

  def random(id: Integer) = new Individual(0, id, (methods filter (_ => Random.generator.nextBoolean) map ((_, Statement.empty))).toMap)

  def fromXML(node: Node) = {
    val generation = (node \@ "generation").toInt
    val id = (node \@ "id").toInt
    new Individual(generation, id, ((node \ "method") map {
      method => ((method \@ "name"), Statement fromXML (method \ "statement").head)
    }).toMap)
  }
}

class Individual(val generation: Integer, val id: Integer, val methods: Map[String, StatementPart]) {
  var fitness = 0;
  val className = "RaGA"
  val subjectPackage = "subject"
  val name = s"${className}_${generation}_$id"
  val fqName = s"$subjectPackage.$name"
  val robotName = s"$fqName*"
  val filePath = s"$subjectPackage/$name.java"

  override def toString = {
    def methodBody(methodKey: String) = methods getOrElse (methodKey, "")
    def methodToString(methodKey: String): String = Individual.methodSignature(methodKey) + methodBody(methodKey) + "}\n"
    val classBody = Individual.methods map methodToString mkString "\n"
    s"package $subjectPackage;\nimport robocode.*;\npublic class $name extends AdvancedRobot {\n$classBody\n}\n"
  }

  private def shuffle(methods: Map[String, StatementPart]): Map[String, StatementPart] = {
    val keys = methods.keySet.toList
    val (candidate1, candidate2) = (keys(Random.generator.nextInt(keys.size)), keys(Random.generator.nextInt(keys.size)))
    val (stmt1, stmt2) = (methods(candidate1), methods(candidate2))
    methods + (candidate1 -> stmt2) + (candidate2 -> stmt1)
  }

  private def mutate(methods: Map[String, StatementPart]): Map[String, StatementPart] = {
    val candidate = Individual.methods(Random.generator.nextInt(Individual.methods.size))
    val stmt = if (methods contains candidate) methods(candidate).mutate else Statement.empty
    val mix = methods + (candidate -> stmt)
    if (Random.generator.nextBoolean) mix else shuffle(mix)
  }

  def reproduceWith(that: Individual, newId: Integer): Individual =
    if (that.fitness > this.fitness) that.reproduceWith(this, newId)
    else {
      val mix = methods.foldLeft(Map.empty[String, StatementPart]) { (acc, m) =>
        if (that.methods contains m._1) {
          if (Random.generator.nextBoolean) acc + (m._1 -> that.methods(m._1))
          else acc + (m._1 -> that.methods(m._1).mixWith(m._2))
        } else acc + m
      }
      new Individual(generation + 1, newId, mutate(mix))
    }

  def toXML: Elem =
    <individual generation={ generation.toString } id={ id.toString } fitness={ fitness.toString }>{ methods.keySet map { method => <method name={ method }>{ methods(method).toXML }</method> } }</individual>
}