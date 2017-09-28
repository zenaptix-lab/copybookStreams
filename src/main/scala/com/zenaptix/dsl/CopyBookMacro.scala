package com.zenaptix.dsl

import scala.collection.immutable.Seq
import scala.meta._

/**
  * Created by rikus on 9/28/17.
  */

class CopyBookMacro extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls @ Defn.Class(_, _, _, Ctor.Primary(_, _, paramss), template) =>
        val namesToValues: Seq[Term.Tuple] = paramss.flatten.map { param =>
          q"(${param.name.syntax}, ${Term.Name(param.name.value)})"
        }
        val toSchemaTypeImpl: Term =
          q"_root_.com.sksamuel.avro4s(..$namesToValues)"
        val toSchemaType =
          q"_root_.com.sksamuel.avro4s(..$namesToValues) = $toSchemaTypeImpl"
        val templateStats: Seq[Stat] = toSchemaType +: template.stats.getOrElse(Nil)
        cls.copy(templ = template.copy(stats = Some(templateStats)))
      case _ =>
        println(defn.structure)
        abort("@Class2Map must annotate a class.")
    }
  }
}
