import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue
    var minY = Float.MaxValue
    var maxX = Float.MinValue
    var maxY = Float.MinValue

    def width = maxX - minX
    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2
    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float
    def massY: Float
    def mass: Float
    def centerX: Float
    def centerY: Float
    def size: Float
    def total: Int
    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass:  Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {
    val centerX: Float = nw.centerX + nw.size/2
    val centerY: Float = nw.centerY + nw.size/2
    val size: Float = size * 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float =  {
      if(mass > 0) (nw.massX * nw.mass +
        ne.massX * ne.mass +
        sw.massX * sw.mass +
        se.massX * se.mass) / mass
      else
        centerX
    }

    val massY: Float = {
      if(mass > 0) (nw.massY * nw.mass +
        ne.massY *  ne.mass +
        sw.massY *  sw.mass +
        se.massY *  se.mass) / mass
      else
        centerY
    }

    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if(b.x <= centerX && b.y <= centerY)
        Fork(nw.insert(b), ne, sw, se)
      else if(b.x <= centerX && b.y > centerY)
        Fork(nw, ne, sw.insert(b), se)
      else if(b.x > centerX && b.y <= centerY)
        Fork(nw, ne.insert(b), sw, se)
      else
        Fork(nw, ne, sw, se.insert(b))
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body]) extends Quad {
    val mass  = bodies.foldLeft(0f)( (res, b) => res + b.mass)
    val massX = bodies.foldLeft(0f)( (res, b) => res + b.x*b.mass) / mass
    val massY = bodies.foldLeft(0f)( (res, b) => res + b.y*b.mass) / mass

    val total: Int = bodies.length

    def insert(bd: Body): Quad = {
      if(size > minimumSize) {
        var fork = Fork(
          Empty(centerX-size/4, centerY-size/4, size/2),
          Empty(centerX+size/4, centerY-size/4, size/2),
          Empty(centerX-size/4, centerY+size/4, size/2),
          Empty(centerX+size/4, centerY+size/4, size/2))

        for(b <- bd+:bodies) {
          if(b.x < centerX && b.y < centerY)
            fork = Fork(fork.nw.insert(b), fork.ne, fork.sw, fork.se)
          else if(b.x < centerX && b.y >= centerY)
            fork = Fork(fork.nw, fork.ne, fork.sw.insert(b), fork.se)
          else if(b.x >= centerX && b.y < centerY)
            fork = Fork(fork.nw, fork.ne.insert(b), fork.sw, fork.se)
          else
            fork = Fork(fork.nw, fork.ne, fork.sw, fork.se.insert(b))
        }

        fork
      }
      else Leaf(centerX, centerY, size, bodies :+ bd)
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) => Unit
        case Leaf(_, _, _, bodies) => bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) =>
          if(quad.size / distance(quad.centerX, quad.centerY, x, y) < theta )
            addForce(quad.mass, quad.massX, quad.massY)
          else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val c = ((b.x - boundaries.minX) / sectorSize).toInt
      val r = ((b.y - boundaries.minY) / sectorSize).toInt
      matrix(r * sectorPrecision + c) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (i <- 0 until matrix.length)
        matrix(i) = matrix(i).combine(that.matrix(i))
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}

/*
   to test more aspects of your code, write your own unit tests.
 - Take another very careful look at the assignment description. Try to find out if you
   misunderstood parts of it. While reading through the assignment, write more tests.

Below you can find a short feedback for every individual test that failed.

======== LOG OF FAILED TESTS ========
Your solution achieved a testing score of 23 out of 63.


[Test Description] Body.updated should consider a Fork as opaque if it is far away
[Observed Error] FloatOps.DoubleOps(body.xspeed.toDouble).~=(-0.33580848574638367) was false xspeed was 0.0
[Lost Points] 2

[Test Description] 'mergeBoundaries' should correctly merge two boundaries
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

[Test Description] Body.updated should take bodies in a Leaf into account
[Observed Error] FloatOps.DoubleOps(body.xspeed.toDouble).~=(12.587037086486816) was false
[Lost Points] 2

[Test Description] 'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

[Test Description] 'SectorMatrix.+=' should add a body at (64,27) to the correct bucket of a sector matrix of size 108 when the sector precision is 12
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

[Test Description] Leaf.insert(b) should return a new Fork if size > minimumSize
[Observed Error] ne of the Fork, Empty(22.5,27.5,5.0), should be a Leaf
[Lost Points] 2

[Test Description] Fork.insert(b) should insert recursively in the appropriate quadrant
[Observed Error] ne of the Fork, Empty(22.5,27.5,5.0), should be a Leaf
[Lost Points] 2

[Test Description] 'insert' should work correctly on a leaf with center (1,1) and size 2
[Observed Error] Fork(Leaf(0.5,0.5,1.0,List(barneshut.package$Body@6580cfdd)),
                      Empty(1.5,0.5,1.0),
                      Empty(0.5,1.5,1.0),
                      Empty(1.5,1.5,1.0)) did not equal

                 Fork(Leaf(0.5,0.5,1.0,List(barneshut.package$Body@6580cfdd)),
                      Leaf(1.5,0.5,1.0,List(barneshut.package$Body@7e0b85f9)),
                      Empty(0.5,1.5,1.0),
                      Empty(1.5,1.5,1.0))

                      expected Fork(Leaf(0.5,0.5,1.0,List(barneshut.package$Body@6580cfdd)),Leaf(1.5,0.5,1.0,List(barneshut.package$Body@7e0b85f9)),Empty(0.5,1.5,1.0),Empty(1.5,1.5,1.0)) found Fork(Leaf(0.5,0.5,1.0,List(barneshut.package$Body@6580cfdd)),Empty(1.5,0.5,1.0),Empty(0.5,1.5,1.0),Empty(1.5,1.5,1.0))
[Lost Points] 2

[Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points in the same sector
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

[Test Description] Body.updated should recursively traverse a Fork close to it
[Observed Error] 0.0 was not greater than or equal to 0.5 Sanity check: the fork is indeed close enough
[Lost Points] 2

[Test Description] updateBodies should correctly update all bodies wrt to a Quad
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

[Test Description] 'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

[Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

[Test Description] 'computeSectorMatrix' should correctly add points to buckets given 7 points within a boundary of size 96
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

[Test Description] Body.updated should not count the interaction with itself in a Leaf
[Observed Error] FloatOps.DoubleOps(body.xspeed.toDouble).~=(12.587037086486816) was false
[Lost Points] 2

[Test Description] updateBodies should be parallel
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

[Test Description] 'updateBoundaries' should correctly update the boundary when invoked repeatedly with points in the range (0,0) to (100,100)
[Observed Error] an implementation is missing
[exception was thrown] detailed error message in debug output section below
[Lost Points] 2

======== TESTING ENVIRONMENT ========
Limits: memory: 256m,  total time: 850s,  per test case time: 240s

======== DEBUG OUTPUT OF TESTING TOOL ========
[test failure log] test name: BarnesHutSuite::'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.package$SectorMatrix.$plus$eq(package.scala:185)
barneshut.BarnesHutSuite$$anonfun$24.apply$mcV$sp(BarnesHutSuite.scala:503)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:413)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:401)
scala.collection.immutable.List.foreach(List.scala:381)


[test failure log] test name: BarnesHutSuite::'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.package$SectorMatrix.$plus$eq(package.scala:185)
barneshut.BarnesHutSuite$$anonfun$25.apply$mcV$sp(BarnesHutSuite.scala:517)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:413)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:401)
scala.collection.immutable.List.foreach(List.scala:381)


[test failure log] test name: BarnesHutSuite::'SectorMatrix.+=' should add a body at (64,27) to the correct bucket of a sector matrix of size 108 when the sector precision is 12::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.package$SectorMatrix.$plus$eq(package.scala:185)
barneshut.BarnesHutSuite$$anonfun$26.apply$mcV$sp(BarnesHutSuite.scala:531)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:413)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:401)
scala.collection.immutable.List.foreach(List.scala:381)


[test failure log] test name: BarnesHutSuite::'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.package$SectorMatrix.$plus$eq(package.scala:185)
barneshut.BarnesHutSuite$$anonfun$27.apply$mcV$sp(BarnesHutSuite.scala:549)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:413)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:401)
scala.collection.immutable.List.foreach(List.scala:381)


[test failure log] test name: BarnesHutSuite::'SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points in the same sector::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.package$SectorMatrix.$plus$eq(package.scala:185)
barneshut.BarnesHutSuite$$anonfun$28.apply$mcV$sp(BarnesHutSuite.scala:579)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:413)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:401)
scala.collection.immutable.List.foreach(List.scala:381)


[test failure log] test name: BarnesHutSuite::'updateBoundaries' should correctly update the boundary given a body at (3,5)::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.Simulator.updateBoundaries(Simulator.scala:15)
barneshut.BarnesHutSuite$$anonfun$29.apply$mcV$sp(BarnesHutSuite.scala:605)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:413)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:401)
scala.collection.immutable.List.foreach(List.scala:381)


[test failure log] test name: BarnesHutSuite::'updateBoundaries' should correctly update the boundary when invoked repeatedly with points in the range (0,0) to (100,100)::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.Simulator.updateBoundaries(Simulator.scala:15)
barneshut.BarnesHutSuite$$anonfun$30$$anonfun$47.apply(BarnesHutSuite.scala:616)
barneshut.BarnesHutSuite$$anonfun$30$$anonfun$47.apply(BarnesHutSuite.scala:616)
scala.collection.LinearSeqOptimized$class.foldLeft(LinearSeqOptimized.scala:124)
scala.collection.immutable.List.foldLeft(List.scala:84)
barneshut.BarnesHutSuite$$anonfun$30.apply$mcV$sp(BarnesHutSuite.scala:616)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)


[test failure log] test name: BarnesHutSuite::'mergeBoundaries' should correctly merge two boundaries::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.Simulator.mergeBoundaries(Simulator.scala:19)
barneshut.BarnesHutSuite$$anonfun$31.apply$mcV$sp(BarnesHutSuite.scala:632)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:413)
org.scalatest.SuperEngine$$anonfun$traverseSubNodes$1$1.apply(Engine.scala:401)
scala.collection.immutable.List.foreach(List.scala:381)


[test failure log] test name: BarnesHutSuite::'computeSectorMatrix' should correctly add points to buckets given 7 points within a boundary of size 96::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.Simulator$$anonfun$computeSectorMatrix$1.apply(Simulator.scala:31)
barneshut.Simulator$$anonfun$computeSectorMatrix$1.apply(Simulator.scala:28)
barneshut.package$TimeStatistics.timed(package.scala:239)
barneshut.Simulator.computeSectorMatrix(Simulator.scala:28)
barneshut.BarnesHutSuite$$anonfun$32.apply$mcV$sp(BarnesHutSuite.scala:646)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)


[test failure log] test name: BarnesHutSuite::'computeSectorMatrix' should correctly work given 5 points within a boundary of size 96 when some points map to the same sector::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.Simulator$$anonfun$computeSectorMatrix$1.apply(Simulator.scala:31)
barneshut.Simulator$$anonfun$computeSectorMatrix$1.apply(Simulator.scala:28)
barneshut.package$TimeStatistics.timed(package.scala:239)
barneshut.Simulator.computeSectorMatrix(Simulator.scala:28)
barneshut.BarnesHutSuite$$anonfun$33.apply$mcV$sp(BarnesHutSuite.scala:666)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)


[test failure log] test name: BarnesHutSuite::computeSectorMatrix should be parallel::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.Simulator$$anonfun$computeSectorMatrix$1.apply(Simulator.scala:31)
barneshut.Simulator$$anonfun$computeSectorMatrix$1.apply(Simulator.scala:28)
barneshut.package$TimeStatistics.timed(package.scala:239)
barneshut.Simulator.computeSectorMatrix(Simulator.scala:28)
barneshut.BarnesHutSuite$$anonfun$34.apply$mcV$sp(BarnesHutSuite.scala:692)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)


[test failure log] test name: BarnesHutSuite::updateBodies should correctly update all bodies wrt to a Quad::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.Simulator$$anonfun$updateBodies$1.apply(Simulator.scala:41)
barneshut.Simulator$$anonfun$updateBodies$1.apply(Simulator.scala:38)
barneshut.package$TimeStatistics.timed(package.scala:239)
barneshut.Simulator.updateBodies(Simulator.scala:38)
barneshut.BarnesHutSuite$$anonfun$35.apply$mcV$sp(BarnesHutSuite.scala:706)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)


[test failure log] test name: BarnesHutSuite::updateBodies should be parallel::2
scala.NotImplementedError: an implementation is missing
scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
barneshut.Simulator$$anonfun$updateBodies$1.apply(Simulator.scala:41)
barneshut.Simulator$$anonfun$updateBodies$1.apply(Simulator.scala:38)
barneshut.package$TimeStatistics.timed(package.scala:239)
barneshut.Simulator.updateBodies(Simulator.scala:38)
barneshut.BarnesHutSuite$$anonfun$36.apply$mcV$sp(BarnesHutSuite.scala:727)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply$mcV$sp(GradingSuite.scala:118)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
ch.epfl.lamp.grading.GradingSuite$$anonfun$test$1.apply(GradingSuite.scala:117)
org.scalatest.Transformer$$anonfun$apply$1.apply$mcV$sp(Transformer.scala:22)
org.scalatest.OutcomeOf$class.outcomeOf(OutcomeOf.scala:85)
org.scalatest.OutcomeOf$.outcomeOf(OutcomeOf.scala:104)
org.scalatest.Transformer.apply(Transformer.scala:22)
org.scalatest.Transformer.apply(Transformer.scala:20)
org.scalatest.FunSuiteLike$$anon$1.apply(FunSuiteLike.scala:166)
org.scalatest.Suite$class.withFixture(Suite.scala:1122)
org.scalatest.FunSuite.withFixture(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$class.invokeWithFixture$1(FunSuiteLike.scala:163)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.FunSuiteLike$$anonfun$runTest$1.apply(FunSuiteLike.scala:175)
org.scalatest.SuperEngine.runTestImpl(Engine.scala:306)
org.scalatest.FunSuiteLike$class.runTest(FunSuiteLike.scala:175)
org.scalatest.FunSuite.runTest(FunSuite.scala:1555)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
org.scalatest.FunSuiteLike$$anonfun$runTests$1.apply(FunSuiteLike.scala:208)
 */