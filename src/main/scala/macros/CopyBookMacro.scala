package macros

import com.sksamuel.avro4s.AvroSchema
import com.zenaptix.dsl.{CBTree, Files, Group}
import scala.io.BufferedSource
import scala.meta._

/**
  * Created by rikus on 9/28/17.
  */

class CopyBookMacro extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
        val typeName = "[Int]".parse[Type].get
        
        val namesToValues: scala.collection.immutable.Seq[Term.Tuple] = paramss.flatten.map { param =>
          q"(${param.name.syntax}, ${Term.Name(param.name.value)})"
        }
        val toSchemaTypeImpl: Term =
          q"_root_.com.sksamuel.avro4s.AvroSchema[${typeName}]"
        val toSchemaType =
          q"def schema: _root_.com.sksamuel.avro4s.AvroSchema[${typeName}] = $toSchemaTypeImpl"
        val templateStats: scala.collection.immutable.Seq[Stat] = toSchemaType +: template.stats.getOrElse(Nil)
        cls.copy(templ = template.copy(stats = Some(templateStats)))
      case _ =>
        println(defn.structure)
        abort("@Class2Map must annotate a class.")
    }
  }
}

@CopyBookMacro
case class Schemas(source: BufferedSource, lines: String, forest: Seq[Group], roots: Seq[CBTree], namespace: String = "com.zenaptix.dsl") {
  def createSchemas = Files.createCaseClasses(forest, namespace) //todo: create case classes should happen as a macro at compile time to make class availible at runtime
  def schema = AvroSchema[String] //todo: inject type from macro
}

