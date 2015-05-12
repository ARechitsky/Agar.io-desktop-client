import javax.swing.Timer
import scala.swing._

object Main extends SimpleSwingApplication {
  //val g = new Game("echo.websocket.org")

  override def top = new MainFrame {
    title = "Agar.io desktop client"
    val g = new Game("213.219.38.70:443")

    val canvas = new GameFieldPanel(g.state)
    val fpsLabel = new Label()
    listenTo(canvas)
    reactions += {
      case NeedMoveEvent(component, x, y) if component == canvas =>
        g.moveTo(x, y)
      case NeedSplit(component) if component == canvas =>
        g split()
      case NeedSpit(component) if component == canvas =>
        g spit()

    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents += canvas
      contents += fpsLabel
    }
    val timer = new Timer(10, Swing.ActionListener(e => {
      canvas update()
    }))
    timer.start()
    val secondTimer = new Timer(1000, Swing.ActionListener(e => {
      g.state fixFpsAndDps()
      fpsLabel.text = "FPS: %d, Data updates count: %d".format(g.state.fps, g.state.dps)
    }))
    secondTimer.start()
    size = new Dimension(1000, 600)
    canvas.requestFocus()
  }
}
