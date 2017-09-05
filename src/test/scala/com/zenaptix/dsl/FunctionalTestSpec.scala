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

  //create forest
  val forest: Seq[Group] = CopyBookSchema(lines).parseTree(EBCDIC())
  val roots = forest.head.traverseAll

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

  println(Console.GREEN + "Create case classes source from AST" + Console.WHITE)
  Files.createCaseClasses(forest)

  println(Console.GREEN + "create AVRO schema from AST" + Console.WHITE)
  val schema: Schema = AvroSchema[Cqsf602w]
  println(schema.toString(true))
  println(schema.getFields)

  println(Console.GREEN + "parse raw data file to an HList of values" + Console.WHITE)
  val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
  //  println(s"Bitvector of input file ${bytes.toBin}")
  println("roots : " + roots.toList)
  val genRecValues: Seq[List[HList]] = Files.rawDataList(bytes, schema, forest)
  //  genRecValues.foreach(lst => println(lst.mkString(" | ")))
  println("GENREC values : " + genRecValues.head)
  genRecValues.head.foreach({
    case head :: HNil => println(s"head : ${head.getClass.toString} ${head.toString}")
    case _ => println("not anything")
  })

  println(Console.GREEN + "create a generic record from case class" + Console.WHITE)
  val origRec = new GenericData.Record(schema)
  println("origRec fields : " + origRec.getSchema.getFields)

  val genRecVal: List[HList] = genRecValues.head.filter(hlst => hlst match {
    case head :: HNil => true
    case _ => false
  })

  println(Console.GREEN + "append decoded values to generic record" + Console.WHITE)
  val finalRec: GenericData.Record = recursiveBuilder(roots.head, roots, origRec, genRecVal.toIterator)
  println(finalRec.toString)

}

