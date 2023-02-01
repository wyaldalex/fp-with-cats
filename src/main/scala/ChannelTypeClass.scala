import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using


trait ByteTypeClassEncoder [A] {
  def encode(a: A) : Array[Byte]
}

trait ChannelByteTypeClass {
  def write[A](obj: A, encoder: ByteTypeClassEncoder[A]) : Unit
}

object FileChannelByTypeClass extends ChannelByteTypeClass {

  override def write[A](obj: A, encoder: ByteTypeClassEncoder[A]): Unit = {

    val bytes: Array[Byte] = encoder.encode(obj)

    Using(new FileOutputStream("test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

//define different encoders:
object IntByteEncoder extends ByteTypeClassEncoder[Int] {
  override def encode(a: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(a)
    bb.array()
  }
}

object StringByteEncoder extends ByteTypeClassEncoder[String] {
  override def encode(a: String): Array[Byte] = a.getBytes()
}

object FullNameByteEncoder extends ByteTypeClassEncoder[FullName] {
  override def encode(a: FullName): Array[Byte] = {
    s"${a.firstName} ${a.lastName}".getBytes()
  }
}

object ChannelTypeClassApp extends App {
  FileChannelByTypeClass.write(FullName("Cersei", "Lannister"), FullNameByteEncoder)
}
