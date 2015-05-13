import scala.swing._

object Main extends SimpleSwingApplication {
  val settingsWindow = new SettingsWindow
  var gameWindow: GameWindow = null

  listenTo(settingsWindow)

  override def top = settingsWindow

  reactions += {
    case NeedToStartGame(a) =>
      gameWindow = new GameWindow(a)
      listenTo(gameWindow)
      settingsWindow.visible = false
      gameWindow.visible = true
    case GameFinished() =>
      deafTo(gameWindow)
      gameWindow.close()
      settingsWindow.visible = true
  }
}
