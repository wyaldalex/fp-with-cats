import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using


trait ByteTypeClassEncoder [A] {
  def encode(a: A) : Array[Byte]
}

trait ChannelByteTypeClass {
  def write[A](obj: A) (implicit encoder: ByteTypeClassEncoder[A]) : Unit
}

object FileChannelByTypeClass extends ChannelByteTypeClass {

  override def write[A](obj: A)(implicit encoder: ByteTypeClassEncoder[A]): Unit = {

    val bytes: Array[Byte] = encoder.encode(obj)

    Using(new FileOutputStream("test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

case class Switch(isOn: Boolean)
//can also be provided in the companion object
object Switch {
  implicit object SwitchEncoder extends ByteTypeClassEncoder[Switch] {
    override def encode(a: Switch): Array[Byte] = {
      a.isOn match {
        case a: Boolean if a == true => Array('1'.toByte)
        case a: Boolean if a == false => Array('0'.toByte)
      }
    }
  }
}

//define different encoders:
object MainEncoder {


  implicit object IntByteEncoder extends ByteTypeClassEncoder[Int] {
    override def encode(a: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(a)
      bb.array()
    }
  }

  implicit object StringByteEncoder extends ByteTypeClassEncoder[String] {
    override def encode(a: String): Array[Byte] = a.getBytes()
  }

  implicit object FullNameByteEncoder extends ByteTypeClassEncoder[FullName] {
    override def encode(a: FullName): Array[Byte] = {
      s"${a.firstName} ${a.lastName}".getBytes()
    }
  }

  implicit val floatEncoder : ByteTypeClassEncoder[Float] = new ByteTypeClassEncoder[Float] {
    override def encode(a: Float): Array[Byte] = Array(a.toByte)
  }

  def instance[A](f: A => Array[Byte]): ByteTypeClassEncoder[A] = {
    new ByteTypeClassEncoder[A] {
      override def encode(a: A): Array[Byte] = f(a)
    }
  }

  implicit val doubleEncoder = instance[Double](d => Array(d.toByte))

  def apply[A](implicit ev: ByteTypeClassEncoder[A]): ByteTypeClassEncoder[A] = ev

}

object ChannelTypeClassApp extends App {
  import MainEncoder._
  //FileChannelByTypeClass.write(FullName("Cersei", "Lannister2"))
  //FileChannelByTypeClass.write(Switch(true))
  FileChannelByTypeClass.write(1921.312)

}
