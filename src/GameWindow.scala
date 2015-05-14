import javax.swing.{JOptionPane, Timer}
import scala.swing._
import scala.swing.event.{ButtonClicked, Event}

case class GameFinished() extends Event

class GameWindow(address: String) extends Frame {

  override def closeOperation() {
    publish(GameFinished())
  }

  val state = new GameState
  val networkAdapter = new NetworkAdapter(address, state)
  val canvas = new GameFieldPanel(state)
  canvas.preferredSize = new Dimension(1000, 600)
  val fpsLabel = new Label()
  val playButton = new Button("Play")
  val nickText = new TextField()

  title = "Agar.io desktop client"

  listenTo(canvas)
  listenTo(networkAdapter)
  listenTo(playButton)
  listenTo(state)
  reactions += {
    case NeedMoveEvent(x, y) =>
      networkAdapter.sendMoveTo(x, y)
    case NeedSplit() =>
      networkAdapter sendSplit()
    case NeedSpit() =>
      networkAdapter sendSpit()
    case ButtonClicked(source) if source == playButton =>
      networkAdapter sendNick nickText.text
    case StateChanged(_, s) =>
      playButton.enabled = s == State.Spectating
    case Disconnected(reason) =>
      JOptionPane.showMessageDialog(null, "Disconnected: " + reason.getReasonPhrase)
      publish(GameFinished())
    case ConnectionError(thr) =>
      JOptionPane.showMessageDialog(null, "Connection error: " + thr.getLocalizedMessage)
      publish(GameFinished())
  }
  contents = new BoxPanel(Orientation.Vertical) {
    contents += canvas
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += fpsLabel
      contents += nickText
      contents += playButton
    }
  }
  val timer = new Timer(10, Swing.ActionListener(e => {
    canvas.update()
  }))
  timer.start()
  val secondTimer = new Timer(1000, Swing.ActionListener(e => {
    state.fixFpsAndDps()
    fpsLabel.text = "FPS: %d, Data updates count: %d".format(state.fps, state.dps)
  }))
  secondTimer.start()
  canvas.requestFocus()

  networkAdapter.connect()
}
