package com.zenaptix.dsl

import com.sksamuel.avro4s.AvroSchema
import org.apache.avro.generic.GenericData
import scodec.bits.BitVector
import shapeless._

import scala.io.{BufferedSource, Source}
import com.zenaptix.dsl.Files._
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import com.zenaptix.macros._
import org.apache.avro.Schema
import reftree.render.{Renderer, RenderingOptions}
import reftree.diagram.Diagram
import java.nio.file.Paths
/**
  * Created by rikus on 9/15/17.
  */

@CopyBookMacro
case class Schemas(source: BufferedSource, lines: String, forest: Seq[Group], roots: Seq[CBTree], namespace: String = "com.zenaptix.dsl.cobolClasses", cobolTypes: String = "com.sksamuel.avro4s.AvroSchema[com.zenaptix.dsl.cobolClasses.Mbsk861]") {
  def createSchemas = Files.createCaseClasses(forest, namespace) //todo: create case classes should happen as a macro at compile time to make class availible at runtime
  //  def schema: Schema = AvroSchema[String] //todo: inject type from macro
}

object Main extends App with LazyLogging {
  val conf = ConfigFactory.load()
  val rendr = renderer("/home/rikus/Desktop")
  import rendr._

  if (args.length == 0) {
    println(Console.RED + "please give input args" + Console.WHITE)
  }
  else {
    val source = Source.fromFile(args(0).toString)
    val lines: String = try source.getLines().mkString("\n") finally source.close()
    val forest = CopyBookSchema(lines).parseTree(EBCDIC())

    Diagram.sourceCodeCaption(forest).render("testViz")

    val roots = forest.head.traverseAll
    val schemas = Schemas(source, lines, forest, roots)

    args(1).toString match {
      case "-C" => {
        schemas.createSchemas
      }

      case "-R" =>
        var bytes: BitVector = Files.copyBytes(args(2))
        logger.info("bytes : " + bytes.slice(0, 100).toBin)
        logger.error("schemas : " + schemas.schema.length)
        var recordCounter = 0
        while (recordCounter < conf.getInt("copybook.numRecords")) {
          var counter = 0
          val schema = schemas.schema //schema function injected with macro
          schema.foreach(sc => {
            logger.error(Console.RED + s"schema : ${sc.toString(true)} " + Console.WHITE)
            val origRec = new GenericData.Record(sc)
            logger.error(Console.RED + s"counter : $counter" + Console.WHITE)
            val genRecValues = Files.rawDataList(args(3).toInt, bytes, sc, forest(counter))
            val genRecVal: List[HList] = genRecValues._1.filter(hlst => hlst match {
              case head :: HNil => true
              case _ => false
            })
            logger.info("genRecordVal : " + genRecVal)
            val finalRec: GenericData.Record = recursiveBuilder(roots.head, roots, origRec, genRecVal.toIterator)
            println(Console.YELLOW + finalRec.toString + Console.WHITE)
            bytes = bytes.drop(genRecValues._2)
            writeValues2File(genRecVal, args(4))
            counter += 1
          })
          recordCounter += 1
        }
    }
  }
}
