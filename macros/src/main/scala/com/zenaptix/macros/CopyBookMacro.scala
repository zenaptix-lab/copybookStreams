package com.zenaptix.macros

import scala.meta._
import scala.annotation.compileTimeOnly
import com.sksamuel.avro4s.AvroSchema

/**
  * Created by rikus on 9/28/17.
  */

class CopyBookMacro extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls@Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
        val namesToValues: Seq[Term.Tuple] = paramss.flatten.map({ param =>
          q"(${Term.fresh(param.name.syntax)}, ${Term.Name(param.name.value)})"
        })
        val typesList = List("com.zenaptix.dsl.cobolClasses.Mbsk861")
        val toSchemaTypeImpl =
        //          q"com.sksamuel.avro4s.AvroSchema[com.zenaptix.dsl.cobolClasses.Mbsk861]"
        //          q"com.sksamuel.avro4s.AvroSchema[${typesList.head.parse[Type].get}]"
          q"List(com.sksamuel.avro4s.AvroSchema[${typesList.head.parse[Type].get}])"
        val toSchemaType =
          q"def schema: List[org.apache.avro.Schema] = $toSchemaTypeImpl"

        val templateStats: scala.collection.immutable.Seq[Stat] = toSchemaType +: template.stats.getOrElse(Nil)
        cls.copy(templ = template.copy(stats = Some(templateStats)))
      case _ =>
        println(defn.structure)
        abort("@CopyBookMacro must annotate a class.")
    }
  }
}
