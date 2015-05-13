import scala.swing._
import scala.swing.event.{ButtonClicked, Event}

case class NeedToStartGame(address: String) extends Event

case class ComboBoxItem(value: String, text: String) {
  override def toString: String = text
}

class SettingsWindow() extends MainFrame {
  val regionsComboBox = new ComboBox[ComboBoxItem](
    List(ComboBoxItem("US-Fremont", "US West"),
      ComboBoxItem("US-Atlanta", "US East"),
      ComboBoxItem("BR-Brazil", "South America"),
      ComboBoxItem("EU-London", "Europe"),
      ComboBoxItem("RU-Russia", "Russia"),
      ComboBoxItem("JP-Tokyo", "East Asia"),
      ComboBoxItem("CN-China", "China"),
      ComboBoxItem("SG-Singapore", "Oceania"))
  )
  regionsComboBox.renderer = ListView.GenericRenderer
  val gameTypesComboBox = new ComboBox[ComboBoxItem](
    List(ComboBoxItem("", "FFA"),
      ComboBoxItem(":teams", "Teams"))
  )
  regionsComboBox.renderer = ListView.GenericRenderer
  val findServerButton = new Button("Find server")
  findServerButton.enabled = false
  val serverText = new TextField("213.168.249.134:443")
  val startButton = new Button("Start")

  title = "Agar.io desktop client"

  listenTo(startButton)
  listenTo(findServerButton)
  reactions += {
    case ButtonClicked(source) if source == startButton =>
      publish(NeedToStartGame(serverText.text))
  }
  contents = new BoxPanel(Orientation.Vertical) {
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += regionsComboBox
      contents += gameTypesComboBox
      contents += findServerButton
    }
    contents += serverText
    contents += startButton
  }
  pack()
}
