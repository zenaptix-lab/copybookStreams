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

  println(Console.GREEN + "parse raw data file to generic record" + Console.WHITE)
  val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
  //  println(s"Bitvector of input file ${bytes.toBin}")
  println("roots : " + roots.toList)
  val genRecValues = Files.rawDataList(bytes, schema, forest)
  //  genRecValues.foreach(lst => println(lst.mkString(" | ")))
  println("GENREC values : " + genRecValues.head)
  genRecValues.head.foreach({
    case head :: HNil => println(s"head : ${head.getClass.toString} ${head.toString}")
    case _ => println("not anything")
  })

  def genRecBuilder(recValues: Seq[List[HList]], forest: Seq[Group], schema: Schema) = {
    val origRec = new GenericData.Record(schema)
    forest.map(tree => {
      val roots = tree.traverseAll
      val rootsItr = roots.toIterator
      var root = rootsItr.next()
      recValues.head.map(recValue => { //foreach record value
        val reqRec = getGenRec(origRec, root, roots).getOrElse(origRec)
        println("ROOT " + root.camelCaseVar)
        println("REQREC : " + reqRec.getSchema.getFields)
        if (rootsItr.hasNext) {
          println(Console.YELLOW + "GENRECBUILDER ROOT " + root + Console.WHITE)
          println("reqRec : " + reqRec.getSchema.getName)
          println("reqRec Fields: " + reqRec.getSchema.getFields)
          recValue match {
            case head :: HNil => {
              println(s"head : ${head.getClass.toString} ${head.toString}")
              reqRec.put(root.camelCaseVar, head)
            }
            case _ => {
              println("not anything")
            }
          }
          root = rootsItr.next()
          reqRec
        }
        else{
          reqRec
        }
      })
    })
  }

  val completeRec = genRecBuilder(genRecValues, forest, schema)
  completeRec.head.toString
  //recursive add to generic record

}

