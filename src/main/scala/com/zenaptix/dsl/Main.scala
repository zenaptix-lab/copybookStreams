package com.zenaptix.dsl

import java.io.File

import com.sksamuel.avro4s.AvroSchema
import org.apache.avro.Schema
import org.apache.avro.generic.GenericData
import scodec.bits.BitVector
import shapeless._

import scala.io.{BufferedSource, Source}
import com.zenaptix.dsl.Files._
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging

import scala.language.experimental.macros

/**
  * Created by rikus on 9/15/17.
  */

object Main extends App with LazyLogging {
  val conf = ConfigFactory.load()
  if (args.length == 0) {
    println(Console.RED + "please give input args" + Console.WHITE)
  }
  else {
    //    val source: BufferedSource = Source.fromFile("/home/rikus/Downloads/mainframe_test/CQSF602.txt")
    val source = Source.fromFile(args(0).toString)
    val lines: String = try source.getLines().mkString("\n") finally source.close()
    val forest: Seq[Group] = CopyBookSchema(lines).parseTree(EBCDIC())
    val bitOffset = 32
    logger.info(Console.CYAN + forest.toList + Console.WHITE)
    //    forest.foreach({ tree =>
    //    logger.info(Console.CYAN + tree.camelCaseVar + Console.WHITE)
    //    val roots = tree.traverseAll

    args(1).toString match {
      case "-C" => Files.createCaseClasses(forest, "com.zenaptix.dsl") //todo: create case classes should happen as a macro at compile time to make class availible at runtime

      case "-R" =>
        var bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/paymentHistory/VEPSS.SLIVE.GSAM.DK86A0.SEP19")
        //          val schema: Schema = AvroSchema[Mbsk862]
        val files = new File("/home/rikus/Documents/ZenAptix/copybookStreams/src/main/resources/schemas").listFiles().reverse
        logger.info("FILES : " + files.toList)
        var recordCounter = 0
        var fileCount = 0
        while (recordCounter < conf.getInt("copybook.numRecords")) {
          files.foreach({ f =>
            val roots = forest(fileCount).traverseAll
            val schema = org.apache.avro.Schema.parse(f)
            logger.error(Console.RED + s"shema : ${schema.toString(true)} " + Console.WHITE)
            val origRec = new GenericData.Record(schema)
            val genRecValues = Files.rawDataList(bitOffset, bytes, schema, forest(fileCount))
            val genRecVal: List[HList] = genRecValues._1.filter(hlst => hlst match {
              case head :: HNil => true
              case _ => false
            })
            logger.info("###########################################genRecVal##################################### : " + genRecVal)
            val finalRec: GenericData.Record = recursiveBuilder(roots.head, roots, origRec, genRecVal.toIterator)
            println(Console.YELLOW + finalRec.toString + Console.WHITE)
            bytes = bytes.drop(genRecValues._2)
            writeValues2File(genRecVal, "/home/rikus/Downloads/mainframe_test/valuesTest.csv")

            fileCount += 1
          })
          recordCounter += 1
        }
    }
  }
}
