package com.github.kpacha.tankbuilder

object Individual {
  val methods = Seq(
//    "public void onBulletHit(BulletHitEvent event) {\n",
//    "public void onBulletHitBullet(BulletHitBulletEvent event) {\n",
//    "public void onBulletMissed(BulletMissedEvent event) {\n",
    "public void onHitByBullet(HitByBulletEvent event) {\n",
    "public void onHitRobot(HitRobotEvent event) {\n",
    "public void onHitWall(HitWallEvent event) {\n",
    "public void onScannedRobot(ScannedRobotEvent event) {\n",
    "public void run() {\n\tsetAdjustGunForRobotTurn(true);\n\tsetAdjustRadarForGunTurn(true);\n\tsetAdjustRadarForRobotTurn(true);\n")

//  def random(id: Integer) = new Individual(0, id, (methods filter (_ => Random.generator.nextBoolean) map ((_, new Statement(Nil)))).toMap)
          
  val startingPointScan = new Statement(List(
      Action("setTurnGunRightRadians", Expression.ExpressionNode(Expression.Input("event.getBearingRadians()"), Expression.ExpressionOp("-"), Expression.Input("getGunHeadingRadians()"))),
      Action("setFire", Expression.Constant(2))))

  def random(id: Integer) = new Individual(0, id, Map(methods(4) -> new Statement(List(Action("turnRadarRightRadians", Expression.Constant(Double.MaxValue)))),
      methods(3) -> startingPointScan))
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
    val heading = s"package $subjectPackage;\nimport robocode.*;\npublic class $name extends AdvancedRobot {\n"
    val body = Individual.methods map (m => m + (methods getOrElse (m, "")) + "}\n") mkString "\n"
    heading + body + "}\n"
  }

  private def mutate(methods: Map[String, StatementPart]): Map[String, StatementPart] = {
    val candidate = Individual.methods.toList(Random.generator.nextInt(Individual.methods.size))
    val stmt = if (methods contains candidate) methods(candidate).mutate else Statement.random
    methods + (candidate -> stmt)
  }

  def reproduceWith(that: Individual, newId: Integer): Individual =
    if (that.fitness > this.fitness) that.reproduceWith(this, newId)
    else {
      val mix = methods.foldLeft(Map.empty[String, StatementPart]) { (acc, m) =>
        if (that.methods contains m._1) {
          if (Random.generator.nextBoolean) acc + (m._1 -> that.methods(m._1))
          else acc + (m._1 -> that.methods(m._1).mixWith(m._2))
        }
        else acc + m
      }
      new Individual(generation + 1, newId, mutate(mix))
    }
}