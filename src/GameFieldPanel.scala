import java.awt
import java.awt.geom.{Rectangle2D, Line2D, Ellipse2D}
import java.awt.{BasicStroke, RenderingHints, Color}
import javax.swing.Timer
import scala.Some
import scala.swing.event.{Key, KeyPressed, Event, MouseMoved}
import scala.swing.{Swing, Panel}

case class NeedMoveEvent(x: Double, y: Double) extends Event

case class NeedSplit() extends Event

case class NeedSpit() extends Event

class GameFieldPanel(state: GameState) extends Panel {
  private val fieldViewMultiplier = 1.3d
  private var currentPoints = state.points
  private var mousePoint: Option[awt.Point] = None
  private var origRatio: Option[Double] = None
  private var ratio = 1d
  private var cx: Option[Double] = None
  private var cy: Option[Double] = None


  listenTo(mouse.moves)
  listenTo(keys)

  reactions += {
    case MouseMoved(_, point, modifiers) =>
      mousePoint = Some(point)
      sendMove()
    case KeyPressed(_, Key.Space, modifiers, _) =>
      publish(NeedSplit())
    case KeyPressed(_, Key.W, modifiers, _) =>
      publish(NeedSpit())
  }

  new Timer(100, Swing.ActionListener(_ => sendMove())).start()

  def sendMove() {
    if (cx == None) return
    mousePoint match {
      case Some(p) =>
        val x = cx.get + (p.x.toDouble - size.width / 2d) / ratio
        val y = cy.get + (p.y.toDouble - size.height / 2d) / ratio
        publish(NeedMoveEvent(x, y))
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
      val newOrigRatio = Math.pow(Math.min(64 / score, 1), 0.4)
      origRatio = Some((origRatio.getOrElse(newOrigRatio) * 9 + newOrigRatio) / 10)
      ratio = origRatio.get * Math.min(size.height / 1080d, size.width / 1920d) / fieldViewMultiplier
      cx = Some((cx.getOrElse(newCx) * 9 + newCx) / 10)
      cy = Some((cy.getOrElse(newCy) * 9 + newCy) / 10)
    }

    state.framesUpdateCount += 1

    repaint()
  }

  override protected def paintComponent(g: _root_.scala.swing.Graphics2D) {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.clearRect(0, 0, size.width, size.height)
    if (cx == None) return
    val oldT = g.getTransform
    g.translate(size.width / 2, size.height / 2)
    g.scale(ratio, ratio)
    g.translate(-cx.get, -cy.get)
    //g.drawOval(cx.get.toInt - 50, cy.get.toInt - 50, 100, 100)
    currentPoints.toList sortBy (_._2.size) foreach {
      case (_, value: Point) => drawPoint(g, value)
    }
    drawFieldBorder(g)
    drawVisibilityBorder(g)
    g.setTransform(oldT)
  }

  private def drawPoint(g: _root_.scala.swing.Graphics2D, point: Point) {
    val oldColor = g.getColor
    val oldStroke = g.getStroke
    val cx = point.x
    val cy = point.y
    val d = point.size * 2
    val color = if (point.isVirus) new Color(0xA0000000 | (point.color.getRGB & 0x00FFFFFF), true) else point.color
    g.setColor(color)
    g.setStroke(new BasicStroke(Math.min(5f, point.size / 10).toFloat))
    g.fill(new Ellipse2D.Double(cx - d / 2, cy - d / 2, d, d))
    g.setColor(color.darker())
    g.draw(new Ellipse2D.Double(cx - d / 2, cy - d / 2, d, d))
    g.setColor(Color.WHITE)
    g.draw(new Line2D.Double(cx - d / 6, cy, cx + d / 6, cy))
    g.draw(new Line2D.Double(cx, cy - d / 6, cx, cy + d / 6))
    g.setColor(oldColor)
    g.setStroke(oldStroke)
  }

  private def drawFieldBorder(g: _root_.scala.swing.Graphics2D) {
    g.setColor(Color.RED)
    val x0 = state.field.xmin
    val y0 = state.field.ymin
    val x1 = state.field.xmax
    val y1 = state.field.ymax
    g.draw(new Rectangle2D.Double(x0, y0, x1 - x0, y1 - y0))
  }

  private def drawVisibilityBorder(g: _root_.scala.swing.Graphics2D) {
    g.setColor(new Color(0, 255, 0, 128))
    val x0 = cx.get - 960d / origRatio.get
    val y0 = cy.get - 540d / origRatio.get
    val x1 = cx.get + 960d / origRatio.get
    val y1 = cy.get + 540d / origRatio.get
    g.draw(new Rectangle2D.Double(x0, y0, x1 - x0, y1 - y0))
  }

}
