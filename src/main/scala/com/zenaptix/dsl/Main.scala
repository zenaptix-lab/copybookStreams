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
import scala.language.experimental.macros

/**
  * Created by rikus on 9/15/17.
  */

object Main extends App {
  val conf = ConfigFactory.load()
  printf("hello %s!", "world")
  if (args.length == 0) {
    println(Console.RED + "please give input args" + Console.WHITE)
  }
  else {
    //    val source: BufferedSource = Source.fromFile("/home/rikus/Downloads/mainframe_test/CQSF602.txt")
    val source = Source.fromFile(args(0).toString)
    val lines: String = try source.getLines().mkString("\n") finally source.close()
    val forest: Seq[Group] = CopyBookSchema(lines).parseTree(EBCDIC())
    val roots = forest.head.traverseAll

    args(1).toString match {
      case "-C" => Files.createCaseClasses(forest, "com.zenaptix.dsl") //todo: create case classes should happen as a macro at compile time to make class availible at runtime
      case "-R" =>
        var bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
        val schema: Schema = AvroSchema[Cqsf602w]
        val origRec = new GenericData.Record(schema)
        var counter = 0
        if (conf.getInt("copybook.numRecords") <= 0) {
          while (bytes.size > 0) {
            val genRecValues = Files.rawDataList(32, bytes, schema, forest)
            val genRecVal: List[HList] = genRecValues.head._1.filter(hlst => hlst match {
              case head :: HNil => true
              case _ => false
            })

            val finalRec: GenericData.Record = recursiveBuilder(roots.head, roots, origRec, genRecVal.toIterator)
            println(Console.YELLOW + finalRec.toString + Console.WHITE)
            bytes = bytes.drop(genRecValues.head._2)
            writeValues2File(genRecVal, "/home/rikus/Downloads/mainframe_test/valuesTest.csv")
            counter += 1
          }
        }
        else {
          while (counter < conf.getInt("copybook.numRecords")) {
            val genRecValues = Files.rawDataList(32, bytes, schema, forest)
            val genRecVal: List[HList] = genRecValues.head._1.filter(hlst => hlst match {
              case head :: HNil => true
              case _ => false
            })

            val finalRec: GenericData.Record = recursiveBuilder(roots.head, roots, origRec, genRecVal.toIterator)
            println(Console.YELLOW + finalRec.toString + Console.WHITE)
            bytes = bytes.drop(genRecValues.head._2)
            writeValues2File(genRecVal, "/home/rikus/Downloads/mainframe_test/valuesTest.csv")
            counter += 1
          }
        }
    }
  }
}