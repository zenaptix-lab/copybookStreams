import java.nio.ByteBuffer

import com.zenaptix.dsl.Files.logger
import com.zenaptix.dsl.Main.args

import scala.collection.immutable.Seq
import scala.io.Source
import scala.meta._
import com.zenaptix.dsl._
import scodec._
import scodec.bits._
import codecs._

import reftree.render.{Renderer, RenderingOptions}
import reftree.diagram.Diagram
import java.nio.file.Paths

"aadfasfd.scala".split("\\.").head

def getFileTypeNames(dir:String = "/home/rikus/Documents/ZenAptix/copybookStreams/core/src/main/scala/com/zenaptix/dsl/cobolClasses",namespace:String = "com.zenaptix.dsl.cobolClasses") = {
  val files = Files.getListOfFiles(dir)
  val names = files.map(fle => {
    namespace ++ "." ++ fle.getName.split("\\.").head
  })
  names
}
getFileTypeNames()

val bits = bin"0000000000010110"
val buf = ByteBuffer.allocate(8)
int16.toString
val decValue = Codec.decode(bits)(int16).require.value.asInstanceOf[Number].doubleValue()
val byteArr = buf.putDouble(decValue).array()
val buf2 = ByteBuffer.wrap(byteArr)

buf2.getDouble.toString

//val renderer = Renderer(
//  renderingOptions = RenderingOptions(density = 75),
//  directory = Paths.get("/home/rikus/Desktop")
//)
import com.zenaptix.dsl.Files.renderer
val renderer = com.zenaptix.dsl.Files.renderer("/home/rikus/Desktop")
import renderer._

case class Person(firstName: String, age: Int)
val personTest = Person("Bob", 42)

Diagram.sourceCodeCaption(personTest).render("testViz")