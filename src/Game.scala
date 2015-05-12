
class Game(address: String) {
  val state = new GameState
  val networkAdapter = new NetworkAdapter(address, state)
  networkAdapter connect()

  networkAdapter sendNick "asf"

  def moveTo(x: Double, y: Double) {
    networkAdapter.sendMoveTo(x, y)
  }

  def split() {
    networkAdapter sendSplit()
  }

  def spit() {
    networkAdapter sendSpit()
  }
}
