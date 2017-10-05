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
  if (args.length == 0) {
    println(Console.RED + "please give input args" + Console.WHITE)
  }
  else {
    //    val source: BufferedSource = Source.fromFile("/home/rikus/Downloads/mainframe_test/CQSF602.txt")
    val source = Source.fromFile(args(0).toString)
    val lines: String = try source.getLines().mkString("\n") finally source.close()
    val forest = CopyBookSchema(lines).parseTree(EBCDIC())
    val roots = forest.head.traverseAll
    val schemas = Schemas(source, lines, forest, roots)

    args(1).toString match {
      case "-C" => {
        schemas.createSchemas
      }

      case "-R" =>
        var bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/paymentHistory/VEPSS.SLIVE.GSAM.DK86A0.SEP19")
        logger.info("bytes : " + bytes.slice(0, 100).toBin)
        logger.error("schemas : " + schemas.schema.length)
        //        val schema = schemas.schema.head
        //        while (counter < conf.getInt("copybook.numRecords")) {
        var counter = 0
        val schema = schemas.schema
        schema.foreach(sc => {
          logger.error(Console.RED + s"schema : ${sc.toString(true)} " + Console.WHITE)
          val origRec = new GenericData.Record(sc)

          //          if (conf.getInt("copybook.numRecords") <= 0) {
          //            while (bytes.size > 0) {
          //              val genRecValues = Files.rawDataList(conf.getInt("copybook.recOffset"), bytes, sc, forest(counter))
          //              val genRecVal: List[HList] = genRecValues._1.filter(hlst => hlst match {
          //                case head :: HNil => true
          //                case _ => false
          //              })
          //
          //              val finalRec: GenericData.Record = recursiveBuilder(roots.head, roots, origRec, genRecVal.toIterator)
          //              println(Console.YELLOW + finalRec.toString + Console.WHITE)
          //              bytes = bytes.drop(genRecValues._2)
          //              writeValues2File(genRecVal, "/home/rikus/Downloads/mainframe_test/valuesTest.csv")
          //              counter += 1
          //            }
          //          }

          logger.error(Console.RED + s"counter : $counter" + Console.WHITE)
          val genRecValues = Files.rawDataList(conf.getInt("copybook.recOffset"), bytes, sc, forest(counter))
          val genRecVal: List[HList] = genRecValues._1.filter(hlst => hlst match {
            case head :: HNil => true
            case _ => false
          })
          logger.info("genRecordVal : " + genRecVal)
          logger.info("genRecordVal : " + genRecVal.toIterator.toList)
          val finalRec: GenericData.Record = recursiveBuilder(roots.head, roots, origRec, genRecVal.toIterator)
          println(Console.YELLOW + finalRec.toString + Console.WHITE)
          bytes = bytes.drop(genRecValues._2)
          writeValues2File(genRecVal, "/home/rikus/Downloads/mainframe_test/valuesTest.csv")
          counter += 1
        })
    }
  }
}
