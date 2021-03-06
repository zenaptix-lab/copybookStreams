package com.zenaptix.dsl

import java.io.{BufferedWriter, File, FileWriter}

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

/**
  * Created by rikus on 9/15/17.
  */

@CopyBookMacro
case class Schemas(source: BufferedSource, lines: String, forest: Seq[Group], roots: Seq[CBTree], namespace: String = "com.zenaptix.dsl.cobolClasses", cobolTypes: String = "com.sksamuel.avro4s.AvroSchema[com.zenaptix.dsl.cobolClasses.Mbsk861]") {
  def cleanOutDir(dir:String) = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        val lof = d.listFiles.filter(fl => !fl.getName.contains("Test")).filter(_.isFile).toList
        if(lof.nonEmpty) {
          lof.foreach(f => {
            f.delete() match {
              case true => println(s"delete file : ${f}")
              case false => throw new Exception("Cannot delete old .scala files from working directory")
            }
          })
        }
      }
    }
  def createSchemas = Files.createCaseClasses(forest, namespace)
  //  def schema: Schema = AvroSchema[String] //todo: inject type from macro
}

/**
  * Main of app
  *
  * @args (0) name of cobol text file
  * @args (1) create [-C] or run [-R] flag
  * @args (2) name of binary file
  * @args (3) bit offset in number of bits between records
  * @args (4) name of .csv file to create with decoded results.
  * @args (5) number of records to decode
  */
object Main extends App with LazyLogging {
  val conf = ConfigFactory.load()
  if (args.length == 0) {
    println(Console.RED + "Please give input args" + Console.WHITE)
    println(Console.GREEN +
      """@args(0) name of cobol text file
        |  @args(1) create [-C] or run [-R] flag
        |  @args(2) name of binary file
        |  @args(3) bit offset in number of bits between records
        |  @args(4) name of .csv file to create with decoded results.
        |  @args(5) number of records to decode""".stripMargin + Console.WHITE)
  }
  else {
    val source = Source.fromFile(args(0).toString)
    val lines: String = try source.getLines().mkString("\n") finally source.close()
    val forest = CopyBookSchema(lines).parseTree(EBCDIC())
    val roots = forest.head.traverseAll
    val schemas = Schemas(source, lines, forest, roots)

    args(1).toString match {
      case "-C" => {
        schemas.cleanOutDir("core/src/main/scala/com/zenaptix/dsl/cobolClasses")
        schemas.createSchemas
      }

      case "-R" =>
        var bytes: BitVector = Files.copyBytes(args(2))
        logger.info("bytes : " + bytes.slice(0, 100).toBin)
        logger.info("schemas : " + schemas.schema.length)
        var recordCounter = 0
        while (recordCounter < args(5).toInt) {
          var counter = 0
          val schema = schemas.schema //schema function injected with macro
          schema.foreach(sc => {
            logger.info(Console.RED + s"schema : ${sc.toString(true)} " + Console.WHITE)
            val origRec = new GenericData.Record(sc)
            logger.info(Console.RED + s"counter : $counter" + Console.WHITE)
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
