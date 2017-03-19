/**
  * Created by cbilgin on 11/7/16.
  */
object PouringTest {

  val problem = new WaterPouring(Vector(4,7))
  print(problem.moves)

  print(problem.pathSets.take(3).toList)

  print(problem.solution(6).toList)


}

