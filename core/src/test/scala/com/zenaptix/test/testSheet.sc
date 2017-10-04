import com.zenaptix.dsl.Main.args

import scala.collection.immutable.Seq
import scala.io.Source
import scala.meta._
import com.zenaptix.dsl._

"aadfasfd.scala".split("\\.").head

def getFileTypeNames(dir:String = "/home/rikus/Documents/ZenAptix/copybookStreams/core/src/main/scala/com/zenaptix/dsl/cobolClasses",namespace:String = "com.zenaptix.dsl.cobolClasses") = {
  val files = Files.getListOfFiles(dir)
  val names = files.map(fle => {
    namespace ++ "." ++ fle.getName.split("\\.").head
  })
  names
}
getFileTypeNames()

