import javax.swing.Timer
import scala.swing._

class Game(address: String) extends MainFrame {
  val state = new GameState
  val networkAdapter = new NetworkAdapter(address, state)
  val canvas = new GameFieldPanel(state)
  val fpsLabel = new Label()

  title = "Agar.io desktop client"

  listenTo(canvas)
  reactions += {
    case NeedMoveEvent(_, x, y) =>
      networkAdapter.sendMoveTo(x, y)
    case NeedSplit(_) =>
      networkAdapter sendSplit()
    case NeedSpit(_) =>
      networkAdapter sendSpit()

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
    state fixFpsAndDps()
    fpsLabel.text = "FPS: %d, Data updates count: %d".format(state.fps, state.dps)
  }))
  secondTimer.start()
  size = new Dimension(1000, 600)
  canvas.requestFocus()

  networkAdapter connect()

  networkAdapter sendNick "asf"

}
