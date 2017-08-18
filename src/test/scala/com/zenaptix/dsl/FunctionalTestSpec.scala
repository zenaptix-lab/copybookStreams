package com.zenaptix.dsl

import java.io.File

import com.sksamuel.avro4s.AvroSchema
import com.zenaptix.dsl.CopyBookResources.roots
import com.zenaptix.test.{Cqsf602w, Svse258NoticeRecord}
import org.apache.avro.Schema
import org.apache.avro.generic.GenericData
import org.scalatest.WordSpec
import scodec.bits.BitVector

import scala.io.{BufferedSource, Source}

/**
  * Created by rikus on 8/10/17.
  */
class FunctionalTestSpec extends WordSpec {

  val source: BufferedSource = Source.fromFile("/home/rikus/Downloads/mainframe_test/CQSF602.txt")
  val lines: String = try source.getLines().mkString("\n") finally source.close()

  //create tree
  val roots: Seq[Group] = CopyBookSchema(lines).parseTree(EBCDIC())
  println(Console.YELLOW + "Roots :  " + roots.mkString("\n") + Console.WHITE)

  roots.foreach { root =>
    val t = root.traverseAll
    println(s"\n\nTRAVERSAL 2 {All}:\n ${t.mkString("\n")}")
  }

  println(Console.GREEN + "Create case classes source from AST" + Console.WHITE)
  Files.createCaseClasses(roots)

  println(Console.GREEN + "create AVRO schema from AST" + Console.WHITE)
  val schema: Schema = AvroSchema[Cqsf602w]
  println(schema.toString(true))
  println(schema.getFields)

  println(Console.GREEN + "parse raw data file to generic record" + Console.WHITE)
  val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")
//  println(s"Bitvector of input file ${bytes.toBin}")
  val genRecBuilder: Seq[GenericData.Record] = Files.rawDataParse(bytes, schema, roots)
  println("Generic record : ")
  genRecBuilder.head.toString
//  genRecBuilder.foreach({
//    rec =>
//      println("newGenRec : " + rec.toString)
//  })
}
