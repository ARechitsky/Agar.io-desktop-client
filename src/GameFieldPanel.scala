import java.awt
import java.awt.{RenderingHints, Color}
import javax.swing.Timer
import scala.Some
import scala.swing.event.{Key, KeyPressed, Event, MouseMoved}
import scala.swing.{Swing, Component, Panel}

case class NeedMoveEvent(component: Component, x: Double, y: Double) extends Event

case class NeedSplit(component: Component) extends Event

case class NeedSpit(component: Component) extends Event

class GameFieldPanel(state: GameState) extends Panel {
  private val fieldViewMultiplier = 1.3d
  private var fieldView = state.field
  private var currentPoints = state.points
  private var mousePoint: Option[awt.Point] = None
  private var origRatio = 1d
  private var ratio = 1d
  private var cx = 0d
  private var cy = 0d


  listenTo(mouse.moves)
  listenTo(keys)

  reactions += {
    case MouseMoved(_, point, modifiers) =>
      mousePoint = Some(point)
      sendMove()
    case KeyPressed(_, Key.Space, modifiers, _) =>
      publish(NeedSplit(this))
    case KeyPressed(_, Key.W, modifiers, _) =>
      publish(NeedSpit(this))
  }

  new Timer(100, Swing.ActionListener(_ => sendMove())).start()

  def sendMove() {
    mousePoint match {
      case Some(p) =>
        val x = fieldView.xmin + p.x.toDouble * ratio
        val y = fieldView.ymin + p.y.toDouble * ratio
        publish(NeedMoveEvent(this, x, y))
      case None =>
    }
  }

  def update() {
    val newCurrentPoints = state.points
    currentPoints = newCurrentPoints map {
      case (k: Int, v: Point) => k -> ((currentPoints withDefault newCurrentPoints)(k) * 9 + v) / 10
    }
    val myPoints = state.myPoints filter currentPoints.contains
    val score = myPoints.foldLeft(0d)((s: Double, i: Int) => s + currentPoints(i).size)
    val newCx = myPoints.foldLeft(0d)((s: Double, i: Int) => s + currentPoints(i).x) / myPoints.size
    val newCy = myPoints.foldLeft(0d)((s: Double, i: Int) => s + currentPoints(i).y) / myPoints.size
    if (score > 0) {
      val newOrigRatio = Math.pow(Math.min(64 / score, 1), -0.4)
      origRatio = (origRatio * 9 + newOrigRatio) / 10
      ratio = origRatio / Math.min(size.height / 1080d, size.width / 1920d) * fieldViewMultiplier
      cx = (cx * 9 + newCx) / 10
      cy = (cy * 9 + newCy) / 10
      //val fvRatio = ratio / 2
      fieldView = Field.createFromCenter(cx, cy, ratio * size.width, ratio * size.height)
      //fieldView = (fieldView * 9 + newFieldView) / 10
    }

    state.framesUpdateCount += 1

    repaint()
  }

  override protected def paintComponent(g: _root_.scala.swing.Graphics2D) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.clearRect(0, 0, size.width, size.height)
    if (fieldView == Field(0, 0, 0, 0)) return
    currentPoints.toList sortBy (_._2.size) foreach {
      case (_, value: Point) => drawPoint(g, value)
    }
    drawFieldBorder(g)
    drawVisibilityBorder(g)
  }

  private def drawPoint(g: _root_.scala.swing.Graphics2D, point: Point) {
    val color = if (point.isVirus) new Color(0xA0000000 | (point.color.getRGB & 0x00FFFFFF), true) else point.color
    g.setColor(color)
    val cx = ((point.x - fieldView.xmin) / ratio).toInt
    val cy = ((point.y - fieldView.ymin) / ratio).toInt
    val d = Math.max((point.size * 2 / ratio).toInt, 1)
    g.fillOval(cx - d / 2, cy - d / 2, d, d)
    g.setColor(color.darker())
    g.drawOval(cx - d / 2, cy - d / 2, d, d)
    g.setColor(Color.WHITE)
    g.drawLine(cx - d / 6, cy, cx + d / 6, cy)
    g.drawLine(cx, cy - d / 6, cx, cy + d / 6)
  }

  private def drawFieldBorder(g: _root_.scala.swing.Graphics2D) {
    g.setColor(Color.RED)
    val x0 = ((state.field.xmin - fieldView.xmin) / ratio).toInt
    val y0 = ((state.field.ymin - fieldView.ymin) / ratio).toInt
    val x1 = ((state.field.xmax - fieldView.xmin) / ratio).toInt
    val y1 = ((state.field.ymax - fieldView.ymin) / ratio).toInt
    g.drawRect(x0, y0, x1 - x0, y1 - y0)
  }

  private def drawVisibilityBorder(g: _root_.scala.swing.Graphics2D) {
    g.setColor(new Color(0, 255, 0, 128))
    val x0 = ((fieldView.xmax - fieldView.xmin - origRatio * 1920d) / 2 / ratio).toInt
    val y0 = ((fieldView.ymax - fieldView.ymin - origRatio * 1080d) / 2 / ratio).toInt
    val x1 = ((fieldView.xmax - fieldView.xmin + origRatio * 1920d) / 2 / ratio).toInt
    val y1 = ((fieldView.ymax - fieldView.ymin + origRatio * 1080d) / 2 / ratio).toInt
    g.drawRect(x0, y0, x1 - x0, y1 - y0)
  }

}
