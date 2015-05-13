import javax.swing.{JOptionPane, Timer}
import scala.swing._
import scala.swing.event.Event

case class GameFinished() extends Event

class GameWindow(address: String) extends Frame {

  override def closeOperation() {
    publish(GameFinished())
  }

  val state = new GameState
  val networkAdapter = new NetworkAdapter(address, state)
  val canvas = new GameFieldPanel(state)
  val fpsLabel = new Label()

  title = "Agar.io desktop client"

  listenTo(canvas)
  listenTo(networkAdapter)
  reactions += {
    case NeedMoveEvent(x, y) =>
      networkAdapter.sendMoveTo(x, y)
    case NeedSplit() =>
      networkAdapter sendSplit()
    case NeedSpit() =>
      networkAdapter sendSpit()
    case Disconnected(reason) =>
      JOptionPane.showMessageDialog(null, "Disconnected: " + reason.getReasonPhrase)
      publish(GameFinished())
    case ConnectionError(thr) =>
      JOptionPane.showMessageDialog(null, "Connection error: " + thr.getLocalizedMessage)
      publish(GameFinished())
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
