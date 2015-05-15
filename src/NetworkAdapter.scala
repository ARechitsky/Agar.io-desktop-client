import java.net.URI
import java.nio.{ByteOrder, ByteBuffer}
import java.util
import javax.websocket._
import javax.websocket.ClientEndpointConfig.Configurator
import scala.swing.event.Event
import scala.swing.{Publisher, Color}
import org.glassfish.tyrus.client.ClientManager

case class Disconnected(reason: CloseReason) extends Event

case class ConnectionError(thr: Throwable) extends Event

class NetworkAdapter(address: String, state: GameState) extends Publisher {
  private def createBuffer(size: Int) = ByteBuffer allocate size order ByteOrder.LITTLE_ENDIAN

  private def Byte2Int(b: Byte) = (b.toInt + 256) % 256

  private val endpoint = {
    val client = ClientManager.createClient

    /*client.getProperties.put(
      ClientProperties.PROXY_URI, "http://localhost:8888"
    )*/
    client.connectToServer(new Endpoint {

      object Handler extends MessageHandler.Whole[ByteBuffer]() {

        override def onMessage(message: ByteBuffer) {
          parse(message)
        }
      }

      override def onOpen(session: Session, config: EndpointConfig) {
        session addMessageHandler Handler

      }

      override def onClose(session: Session, closeReason: CloseReason) {
        publish(Disconnected(closeReason))
      }

      override def onError(session: Session, thr: Throwable) {
        publish(ConnectionError(thr))
      }
    }, ClientEndpointConfig.Builder.create.configurator(new Configurator {
      override def beforeRequest(headers: util.Map[String, util.List[String]]) {
        headers.put("Origin", util.Arrays.asList("http://agar.io"))
      }
    }).build,
      URI create ("ws://" + address)).getBasicRemote
  }

  private def sendBinary(b: ByteBuffer) {
    b.rewind()
    endpoint sendBinary b
  }

  private def sendText(t: String) {
    endpoint sendText t
  }

  def connect() {
    sendBinary(createBuffer(5) put 255.toByte putInt 1)
  }

  def sendNick(nick: String) {
    val m = createBuffer(1 + 2 * nick.length)
    m put 0.toByte
    nick foreach m.putChar
    sendBinary(m)
  }

  def parse(m: ByteBuffer) {
    val message = m order ByteOrder.LITTLE_ENDIAN
    def readName(): String = {
      val ret = new StringBuilder
      var ch = message.getChar
      while (ch != 0) {
        ret += ch
        ch = if (message.hasRemaining) message.getChar else 0
      }
      ret.toString()
    }

    message.get match {
      case 16 =>
        //println("Message #16: Current situation")
        val killsCount = message.getShort
        //printf("\t%d kills:\n", killsCount)
        for (i <- 1 to killsCount) {
          val id1 = message.getInt
          val id2 = message.getInt
          //printf("\t\t%d eat %d\n", id1, id2)
          state.myPoints = state.myPoints - id2
        }

        var newPoints = Map[Int, Point]()
        //println("\tChanged blobs:")
        var id = m.getInt
        while (id != 0) {
          //printf("\tId: %d\n", id)
          val x = m.getFloat
          val y = m.getFloat
          val size = m.getFloat
          //printf("\t\tX:%12.6f, Y:%12.6f, size:%10.6f\n", x, y, size)
          //val dumb = m.get
          //printf("\t\tMagic byte1: %d\n", dumb)
          val r = m.get
          val g = m.get
          val b = m.get
          //printf("\t\tColor: #%02x%02x%02x\n", r, g, b)
          val magicByte = m.get
          //printf("\t\tMagic byte2: %d\n", magicByte)
          val isVirus = (magicByte & 1) > 0
          //printf("\t\tIsVirus: %d\n", magicByte & 1)
          if ((magicByte & 2) > 0) m.getInt
          if ((magicByte & 4) > 0) m.getLong
          if ((magicByte & 8) > 0) {
            m.getLong
            m.getLong
          }
          val name = readName()
          //printf("\t\tName: '%s'\n", name)
          newPoints = newPoints + (id -> new Point(x, y, size, new Color(Byte2Int(r), Byte2Int(g), Byte2Int(b)), isVirus, name))

          id = m.getInt
        }

        m.getShort
        val pointsCount = m.getInt
        //printf("\tVisible blobs (%d):\n", pointsCount)
        for (i <- 1 to pointsCount) {
          val id = m.getInt
          newPoints = newPoints + (id -> state.points(id))
          //printf("\t\t%d\n", id)
        }
        state setPoints newPoints

      case 20 =>
        println("Message #20: You are died")
      case 32 =>
        //println("Message #32: Adding blob")
        val id = message.getInt
        state.myPoints = state.myPoints + id
      //printf("\tId: %d\n", id)
      case 49 =>
        //println("Message #49: Top 10")
        var newTop = List[TopRecord]()
        val c = message.getInt
        for (_ <- 1 to c) {
          val id = message.getInt
          val name = readName()
          newTop = newTop :+ TopRecord(id, name)
          //printf("\t%-8d: %s\n", id, name)
        }
        state.top = newTop
      case 50 =>
        var newTeamSizes = List[Double]()
        val c = message.getInt
        for (_ <- 1 to c) {
          val size = message.getFloat.toDouble
          newTeamSizes = newTeamSizes :+ size
        }
        state.teamSizes = newTeamSizes
      case 64 =>
        //println("Message #64: Field bounds")
        val left = m.getDouble
        val top = m.getDouble
        val right = m.getDouble
        val bottom = m.getDouble
        state.field = Field(left, top, right, bottom)
        state.hasField = true
        state.updateState()
      //printf("\tX:%12.6f-%12.6f\n", left, right)
      //printf("\tY:%12.6f-%12.6f\n", top, bottom)
      case x =>
        printf("Message #%d\n", x)
    }
  }

  def sendMoveTo(x: Double, y: Double) {
    sendBinary(createBuffer(21) put 16.toByte putDouble x putDouble y putInt 0)
  }

  def sendSplit() {
    sendBinary(createBuffer(1) put 17.toByte)
  }

  def sendSpit() {
    sendBinary(createBuffer(1) put 21.toByte)
  }
}
