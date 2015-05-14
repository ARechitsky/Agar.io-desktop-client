import scala.swing.event.Event
import scala.swing.{Publisher, Color}

case class Field(xmin: Double, ymin: Double, xmax: Double, ymax: Double)

case class Point(x: Double, y: Double, size: Double, color: Color, isVirus: Boolean, name: String) {
  def *(m: Double) = Point(x * m, y * m, size * m, color, isVirus, name)

  def /(m: Double) = *(1 / m)

  def +(o: Point) = Point(x + o.x, y + o.y, size + o.size, color, isVirus, name)
}

case class TopRecord(id: Int, name: String)

case class StateChanged(oldState: State.State, newState: State.State) extends Event

object State extends Enumeration {
  type State = Value
  val NotInitialized, Spectating, Playing = Value
}

class GameState extends Publisher {
  var state = State.NotInitialized
  var points = Map[Int, Point]()
  var field = Field(0, 0, 0, 0)
  var myPoints = Set[Int]()
  var top: List[TopRecord] = null
  var teamSizes: List[Double] = null

  var dataUpdateCount = 0
  var framesUpdateCount = 0

  var fps = 0
  var dps = 0

  var hasField = false
  var hasPoints = false

  def updateState() {
    val oldState = state
    var changed = true
    state = state match {
      case State.NotInitialized if hasField && hasPoints =>
        State.Spectating
      case State.Spectating if myPoints.nonEmpty =>
        State.Playing
      case State.Playing if myPoints.isEmpty =>
        State.Spectating
      case _ =>
        changed = false
        state
    }
    if (changed) publish(StateChanged(oldState, state))
  }

  def setPoints(newPoints: Map[Int, Point]) {
    points = newPoints
    dataUpdateCount += 1
    hasPoints = true
    updateState()
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
    points foreach {
      case (k, v) => printf("\t%c%8d: %s\n", if (myPoints contains k) '*' else ' ', k, v.toString)
    }
    println("Top:")
    top foreach println
  }
}
