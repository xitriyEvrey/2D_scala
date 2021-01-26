import java.util.{Locale, Scanner}


object Main extends App{

  Locale.setDefault(Locale.US)
  val s : Scanner = new Scanner(System.in)

  val n : Int = s.nextInt()
  val P : Array[Point] = Array.tabulate[Point](n)(_ => Point(s.nextDouble(), s.nextDouble()))

  val k : Int = s.nextInt()
  val R : Array[Rect] = Array.tabulate[Rect](k)(_ => Rect(Point(s.nextDouble(), s.nextDouble()), Point(s.nextDouble(), s.nextDouble())))

  val Grid = Array.tabulate[Seq[Point]](200, 200)((_, _) => Seq())


  //Grid(0)(0) = Grid(0)(0) :+ Point(1, 1)

  BuildGrid()

  for (i <- R.indices){
    println(GetNumberOfPoints(R(i)))
  }






  def GetInt(a : Double): Int = (a/10).toInt + (if(a < 0) -1 else 0)

  def BuildGrid(): Unit ={
    for (i <- P.indices){
      Grid(GetInt(P(i).x) + 100)(GetInt(P(i).y) + 100).:+(P(i))
    }
  }

  def F(points: Seq[Point], x1 : Double, x2 : Double, y1 : Double, y2 : Double): Int ={
    var n : Int = 0
    for (i <- points.indices){
      if (x1 <= points(i).x && points(i).x <= x2 && y1 <= points(i).y && points(i).y <= y2) n += 1
    }
    n
  }

  def GetNumberOfPoints(r : Rect): Int ={
    var n : Int = 0
    val leftIndex = GetInt(r.P1.x)
    val rightIndex = GetInt(r.P2.x)
    val lowerIndex = GetInt(r.P1.y)
    val upperIndex = GetInt(r.P2.y)
    for (i <- leftIndex to rightIndex){
      for (j <- lowerIndex to upperIndex){
        if (i == leftIndex || i == rightIndex || j == lowerIndex || j == upperIndex){
          n += F(Grid(i)(j), r.P1.x, r.P2.x, r.P1.y, r.P2.y)
        }else{
          n += Grid(i)(j).length
        }
      }
    }
    n
  }

  //////////////
  case class Point(x : Double, y : Double)

  /////////////
  case class Rect(P1 : Point, P2 : Point)
}
