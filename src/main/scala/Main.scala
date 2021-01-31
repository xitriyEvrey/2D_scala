import java.util.{Locale, Scanner}


object Main extends App{
  val size : Int = 10

  Locale.setDefault(Locale.US)
  val s : Scanner = new Scanner(System.in)

  val n : Int = s.nextInt()
  val P : Array[Point] = Array.tabulate[Point](n)(_ => Point(s.nextDouble(), s.nextDouble()))

  val k : Int = s.nextInt()
  val R : Array[Rect] = Array.tabulate[Rect](k)(_ => Rect(s.nextDouble(), s.nextDouble(), s.nextDouble(), s.nextDouble()))

  val Grid = Array.tabulate[Seq[Point]](2 * 1000 / size, 2 * 1000 / size)((_, _) => Seq())

  BuildGrid()

  for (i <- R.indices){
    println(GetNumberOfPoints(R(i)))
  }




  def GetIndex(a : Double): Int = (a/size).toInt + (if(a < 0) -1 else 0) + 1000 / size

  def BuildGrid(): Unit ={
    for (i <- P.indices){
      Grid(GetIndex(P(i).x))(GetIndex(P(i).y)) :+= P(i)
    }
  }

  def CountPoints(points: Seq[Point], x1 : Double, x2 : Double, y1 : Double, y2 : Double): Int ={
    var n : Int = 0
    for (i <- points.indices){
      if (x1 <= points(i).x && points(i).x <= x2 && y1 <= points(i).y && points(i).y <= y2) n += 1
    }
    n
  }

  def GetNumberOfPoints(r : Rect): Int ={
    var n : Int = 0
    val leftIndex = GetIndex(r.x1)
    val rightIndex = GetIndex(r.x2)
    val lowerIndex = GetIndex(r.y1)
    val upperIndex = GetIndex(r.y2)
    for (i <- leftIndex to rightIndex){
      for (j <- lowerIndex to upperIndex){
        if (i == leftIndex || i == rightIndex || j == lowerIndex || j == upperIndex){
          n += CountPoints(Grid(i)(j), r.x1, r.x2, r.y1, r.y2)
        }else{
          n += Grid(i)(j).length
        }
      }
    }
    n
  }

  //////////////
  case class Point(x : Double, y : Double)

  case class Rect(x1 : Double, y1 : Double, x2 : Double, y2 : Double)
}
