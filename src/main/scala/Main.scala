import java.util.Scanner

import scala.Array.ofDim



object Main extends App{

  val s : Scanner = new Scanner(System.in)

  val n : Int = s.nextInt()
  val P : Array[Point] = Array.iterate[Point](Point(s.nextDouble(), s.nextDouble()), n)(_ => Point(s.nextDouble(), s.nextDouble()))

  val k : Int = s.nextInt()
  val R : Array[Rect] = Array.iterate[Rect](Rect(Point(s.nextDouble(), s.nextDouble()), Point(s.nextDouble(), s.nextDouble())), k)(_ => Rect(Point(s.nextDouble(), s.nextDouble()), Point(s.nextDouble(), s.nextDouble())))

  val Grid = ofDim[Int](200, 200)
  BuildGrid()






  def GetInt(a : Double): Int = (a/10).toInt + (if(a < 0) -1 else 0)

  def BuildGrid(): Unit ={
    for (i <- P.indices){
      Grid(GetInt(P(i).x))(GetInt(P(i).y)) += 1
    }
  }

  def GetNumberOfPoints(r : Rect): Unit ={
    var n : Int = 0
    val leftIndex = GetInt(r.P1.x)
    val rightIndex = GetInt(r.P2.x)
    val lowerIndex = GetInt(r.P1.y)
    val upperIndex = GetInt(r.P2.y)
    for (i <- leftIndex to rightIndex){
      for (j <- lowerIndex to upperIndex){
        n += Grid(i)(j)
      }
    }

  }

  //////////////
  case class Point(x : Double, y : Double)

  /////////////
  case class Rect(P1 : Point, P2 : Point)
}