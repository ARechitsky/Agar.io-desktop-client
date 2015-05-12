import scala.swing.Color

case class Field(xmin: Double, ymin: Double, xmax: Double, ymax: Double)

object Field {
  def createFromCenter(cx: Double, cy: Double, width: Double, height: Double) =
    Field(cx - width / 2, cy - height / 2, cx + width / 2, cy + height / 2)
}

case class Point(x: Double, y: Double, size: Double, color: Color, isVirus: Boolean, name: String) {
  def *(m: Double) = Point(x * m, y * m, size * m, color, isVirus, name)

  def /(m: Double) = *(1 / m)

  def +(o: Point) = Point(x + o.x, y + o.y, size + o.size, color, isVirus, name)
}

case class TopRecord(id: Int, name: String)

class GameState extends Mutable {
  var points = Map[Int, Point]()
  var field = Field(0, 0, 0, 0)
  var myPoints = Set[Int]()
  var top = List[TopRecord]()

  var dataUpdateCount = 0
  var framesUpdateCount = 0

  var fps = 0
  var dps = 0

  def setPoints(newPoints: Map[Int, Point]) {
    points = newPoints
    dataUpdateCount += 1
  }

  def fixFpsAndDps() {
    fps = framesUpdateCount
    dps = dataUpdateCount
    framesUpdateCount = 0
    dataUpdateCount = 0
  }

  def print() {
    printf("Field: %f-%fx%f-%f\n", field.xmin, field.xmax, field.ymin, field.ymax)
    println("Points:")
    points foreach ((x: (Int, Point)) => {
      printf("\t%c%8d: %s\n", if (myPoints contains x._1) '*' else ' ', x _1, x._2 toString)
    })
    println("Top:")
    top foreach println
  }
}
