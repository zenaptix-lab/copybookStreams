package com.zenaptix.dsl

import java.io.File

import com.sksamuel.avro4s.AvroSchema

import org.apache.avro.Schema
import org.apache.avro.generic.GenericData
import org.scalatest.WordSpec
import scodec.bits.BitVector
import shapeless._
import scala.io.{BufferedSource, Source}
import com.zenaptix.dsl.Files._
import com.typesafe.config.ConfigFactory

class FunctionalTestSpec extends WordSpec {
  import FunctionalTestSpecResources._

  //create forest
  "a Cobol copybook" can {
    "Traverse the Scala AST by looking at parent" in {
      forest.foreach { tree =>
        val t = tree.traverseGroups
        println(s"\n\nTRAVERSAL 1 {Groups}:\n ${t.mkString("\n")}")
      }
      forest.foreach { tree =>
        val t = tree.traverseGroups
        t.foreach({ grp =>
          println("group : " + grp.camelCaseVar)
          println("children : " + grp.children.toList)
        })
      }
      forest.foreach { tree =>
        val t = tree.traverseAll
        println(s"\n\nTRAVERSAL 2 {All}:\n ${t.mkString("\n")}")
      }
    }

    "Traverse the Scala AST" in {
      forest.foreach { tree =>
        val t = tree.traverseGroups
        println(s"\n\nTRAVERSAL 1 {Groups}:\n ${t.mkString("\n")}")
      }
      forest.foreach { tree =>
        val t = tree.traverseGroups
        t.foreach({ grp =>
          println("group : " + grp.camelCaseVar)
          println("children : " + grp.children.toList)
        })
      }
      forest.foreach { tree =>
        val t = tree.traverseAll
        println(s"\n\nTRAVERSAL 2 {All}:\n ${t.mkString("\n")}")
      }
    }

    "Create case classes source from AST" in {
      Files.createCaseClasses(forest)
    }

    "Create AVRO schema from AST" in {
      val schema: Schema = AvroSchema[Cqsf602w]
      println(schema.toString(true))
      println(schema.getFields)
    }
    "Parse raw data file to an HList of values" in {
      val schema: Schema = AvroSchema[Cqsf602w]
      val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
      //  println(s"Bitvector of input file ${bytes.toBin}")
      println("roots : " + roots.toList)
      val genRecValues = Files.rawDataList(32, bytes, schema, forest)
      //  genRecValues.foreach(lst => println(lst.mkString(" | ")))
      println("GENREC values : " + genRecValues.head)
      genRecValues.head._1.foreach({
        case head :: HNil => println(s"head : ${head.getClass.toString} ${head.toString}")
        case _ => println("not anything")
      })
    }

    "Create a generic record from case class" in {
      val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
      val schema: Schema = AvroSchema[Cqsf602w]
      val genRecValues = Files.rawDataList(32, bytes, schema, forest)
      val origRec = new GenericData.Record(schema)
      println("origRec fields : " + origRec.getSchema.getFields)

      val genRecVal: List[HList] = genRecValues.head._1.filter(hlst => hlst match {
        case head :: HNil => true
        case _ => false
      })

    }
    "Append decoded values to generic record" in {
      val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
      val schema: Schema = AvroSchema[Cqsf602w]
      val origRec = new GenericData.Record(schema)
      val genRecValues = Files.rawDataList(32, bytes, schema, forest)
      println("origRec fields : " + origRec.getSchema.getFields)

      val genRecVal: List[HList] = genRecValues.head._1.filter(hlst => hlst match {
        case head :: HNil => true
        case _ => false
      })
      val finalRec: GenericData.Record = recursiveBuilder(roots.head, roots, origRec, genRecVal.toIterator)
      println(finalRec.toString)
    }
  }
  "a cobol copybook" should {
    "decode multiple records from the same binary file" in {
      var bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
      val schema: Schema = AvroSchema[Cqsf602w]
      val origRec = new GenericData.Record(schema)
      var counter = 0
      if (conf.getInt("copybook.numRecords") <= 0) {
        while (bytes.size > 0) {
          //      while(counter < 10) {
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