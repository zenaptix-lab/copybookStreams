package com.zenaptix.dsl

import java.io.File

import com.sksamuel.avro4s.AvroSchema
import com.zenaptix.test.Cqsf602w
import org.apache.avro.Schema
import org.apache.avro.generic.GenericData
import org.scalatest.WordSpec
import scodec.bits.BitVector
import shapeless._

import scala.io.{BufferedSource, Source}
import com.zenaptix.dsl.Files._

/**
  * Created by rikus on 8/10/17.
  */
class FunctionalTestSpec extends WordSpec {

  val source: BufferedSource = Source.fromFile("/home/rikus/Downloads/mainframe_test/CQSF602.txt")
  val lines: String = try source.getLines().mkString("\n") finally source.close()
  val forest: Seq[Group] = CopyBookSchema(lines).parseTree(EBCDIC())
  val roots = forest.head.traverseAll

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
      val genRecValues: Seq[List[HList]] = Files.rawDataList(32,bytes, schema, forest)
      //  genRecValues.foreach(lst => println(lst.mkString(" | ")))
      println("GENREC values : " + genRecValues.head)
      genRecValues.head.foreach({
        case head :: HNil => println(s"head : ${head.getClass.toString} ${head.toString}")
        case _ => println("not anything")
      })
    }

    "Create a generic record from case class" in {
      val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
      val schema: Schema = AvroSchema[Cqsf602w]
      val genRecValues: Seq[List[HList]] = Files.rawDataList(32,bytes, schema, forest)
      val origRec = new GenericData.Record(schema)
      println("origRec fields : " + origRec.getSchema.getFields)

      val genRecVal: List[HList] = genRecValues.head.filter(hlst => hlst match {
        case head :: HNil => true
        case _ => false
      })

    }
    "Append decoded values to generic record" in {
      val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
      val schema: Schema = AvroSchema[Cqsf602w]
      val origRec = new GenericData.Record(schema)
      val genRecValues: Seq[List[HList]] = Files.rawDataList(32,bytes, schema, forest)
      println("origRec fields : " + origRec.getSchema.getFields)

      val genRecVal: List[HList] = genRecValues.head.filter(hlst => hlst match {
        case head :: HNil => true
        case _ => false
      })
      val finalRec: GenericData.Record = recursiveBuilder(roots.head, roots, origRec, genRecVal.toIterator)
      println(finalRec.toString)
    }
  }
}