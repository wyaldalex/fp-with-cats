import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using


trait ByteEncodable {
  def encode() : Array[Byte]
}

trait ChannelByteEncodable {
  def write(obj: ByteEncodable) : Unit
}

case class FullName(firstName: String, lastName: String) extends ByteEncodable {
  override def encode(): Array[Byte] = firstName.getBytes() ++ lastName.getBytes()
}

object FileChannelByEncodable extends ChannelByteEncodable {
  override def write(obj: ByteEncodable): Unit = {
    val bytes: Array[Byte] = obj.encode()

    Using(new FileOutputStream("test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

object ChannelInheritanceApp extends App {

  FileChannelByEncodable.write(FullName("John", "Locke"))
}
