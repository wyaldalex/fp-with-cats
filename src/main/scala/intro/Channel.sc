import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait Channel {
  def write(obj: Any) : Unit
}

object FileChannel extends Channel {
  override def write(obj: Any): Unit = {
    val bytes: Array[Byte] = obj match {
      case n: Int =>
        val bb = ByteBuffer.allocate(4)
        bb.putInt(n)
        bb.array()
      case n: String =>
        n.getBytes()
      case _ => throw new Exception("Unhandled type")

    }

    Using(new FileOutputStream("test2.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

FileChannel.write("HelloX")