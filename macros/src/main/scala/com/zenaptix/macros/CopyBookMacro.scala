package com.zenaptix.macros

import java.io.File

import scala.meta._
import scala.annotation.compileTimeOnly
import com.sksamuel.avro4s.AvroSchema


/**
  * Created by rikus on 9/28/17.
  */
object CopyBookMacro{
  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
  def getFileTypeNames(dir:String = "/home/rikus/Documents/ZenAptix/copybookStreams/core/src/main/scala/com/zenaptix/dsl/cobolClasses",namespace:String = "com.zenaptix.dsl.cobolClasses" ) = {
    val files = getListOfFiles(dir)
    val names = files.map(fle => {
      namespace ++ "." ++ fle.getName.split("\\.").head
    })
    names
  }
}
class CopyBookMacro extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
        val namesToValues: Seq[Term.Tuple] = paramss.flatten.map({ param =>
          q"(${Term.fresh(param.name.syntax)}, ${Term.Name(param.name.value)})"
        })
//        val typesList = List("com.zenaptix.dsl.cobolClasses.Mbsk861", "com.zenaptix.dsl.cobolClasses.Mbsk862")
        val typesList = CopyBookMacro.getFileTypeNames()

        val toSchemaTypeImpl =
        //          q"com.sksamuel.avro4s.AvroSchema[com.zenaptix.dsl.cobolClasses.Mbsk861]"
        //          q"com.sksamuel.avro4s.AvroSchema[${typesList.head.parse[Type].get}]"
          q"List(com.sksamuel.avro4s.AvroSchema[${typesList.head.parse[Type].get}])"
        val listOfTypes = typesList.map(tpe => {
          q"com.sksamuel.avro4s.AvroSchema[${tpe.parse[Type].get}]"
        })
        val toSchemaType =
          q"""def schema: List[org.apache.avro.Schema] = List(..$listOfTypes)"""

        val templateStats: scala.collection.immutable.Seq[Stat] = toSchemaType +: template.stats.getOrElse(Nil)
        cls.copy(templ = template.copy(stats = Some(templateStats)))
      case _ =>
        println(defn.structure)
        abort("@CopyBookMacro must annotate a class.")
    }
  }
}
