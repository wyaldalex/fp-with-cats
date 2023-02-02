import java.io.{FileInputStream, FileOutputStream}
import java.nio.ByteBuffer
import scala.language.postfixOps
import scala.util.Using


trait ByteTypeClassEncoder [A] {
  def encode(a: A) : Array[Byte]
}


trait ByteTypeClassDecoder [A] {
  def decode(bytes: Array[Byte]) : Option[A]
}

trait ChannelByteTypeClass {
  def write[A](obj: A) (implicit encoder: ByteTypeClassEncoder[A]) : Unit
  def read[A]()(implicit dec: ByteTypeClassDecoder[A]) : A
}

object FileChannelByTypeClass extends ChannelByteTypeClass {

  override def write[A](obj: A)(implicit encoder: ByteTypeClassEncoder[A]): Unit = {

    val bytes: Array[Byte] = encoder.encode(obj)

    Using(new FileOutputStream("test.txt")) { os =>
      os.write(bytes)
      os.flush()
    }
  }

  override def read[A]()(implicit dec: ByteTypeClassDecoder[A]): A = {
    val is = new FileInputStream("test.txt")
    //Iterator continually is.read takeWhile (-1 !=) map (_.toByte) toArray
    val byteArray = Iterator.continually(is.read()).takeWhile(-1 !=).map(_.toByte).toArray
    dec.decode(byteArray).get
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

object MainDecoder {
//  implicit object StringDecoder extends ByteTypeClassDecoder[String] {
//    override def decode(bytes: Array[Byte]): Option[String] = {
//      val convertedString = new String(bytes)
//      convertedString match {
//        case x: String => Some(convertedString)
//        case _ => None
//      }
//    }
//  }

  implicit val stringDecoder = instance[String](bytes => {
    val convertedString = new String(bytes)
    convertedString match {
      case x: String => Some(x)
      case _ => None
    }
  })

  def instance[A](f: Array[Byte] => Option[A]): ByteTypeClassDecoder[A] = {
    new ByteTypeClassDecoder[A] {
      override def decode(bytes: Array[Byte]): Option[A] = f(bytes)
    }
  }

  def apply[A](implicit de: ByteTypeClassDecoder[A]): ByteTypeClassDecoder[A] = de
}

object ChannelTypeClassApp extends App {
  import MainEncoder._
  import MainDecoder._
  FileChannelByTypeClass.write("Cersei Lannister2")
  //FileChannelByTypeClass.write(FullName("Cersei", "Lannister2"))
  //FileChannelByTypeClass.write(Switch(true))
  //FileChannelByTypeClass.write(1921.312)
  println(FileChannelByTypeClass.read())

}
