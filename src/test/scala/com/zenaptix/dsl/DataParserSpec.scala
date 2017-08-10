package com.zenaptix.dsl

import java.io.File

import org.scalatest.WordSpec

import scala.io.{BufferedSource, Source}
import com.sksamuel.avro4s._
import com.zenaptix.test.Svse258NoticeRecord
import org.apache.avro.Schema
import org.apache.avro.file.{DataFileReader, DataFileWriter}
import org.apache.avro.generic.{GenericData, GenericDatumReader, GenericDatumWriter, GenericRecord}
import org.apache.avro.specific.SpecificDatumWriter
/**
  * Created by rikus on 8/10/17.
  */
class DataParserSpec extends WordSpec {

  import com.zenaptix.dsl.CopyBookResources._

  "the data parser" should {
    "create AVRO schema from AST" in {
      println(Console.GREEN + "create AVRO schema from AST" + Console.WHITE)

      roots.foreach {
        root =>
          val schema = AvroSchema[Svse258NoticeRecord]
          root.printToFile(new File(s"src/test/resources/${
            root.camelCased
          }.avsc")) {
            p =>
              p.println(schema.toString(true))
          }
          println(schema.toString(true))
      }
    }

    "parse AVRO schema from file and serialize a generic record" in {
      println(Console.GREEN + "parse AVRO schema from file and serialize a generic record" + Console.WHITE)

      val parsed = new Schema.Parser().parse(new File("src/test/resources/Svse258NoticeRecord.avsc"))
      println(parsed.toString(true))
      println(parsed.getField("svse258RecLength"))
      val genRec = new GenericData.Record(parsed)
      genRec.put("svse258RecLength", 3)
      println("Fields : " + parsed.getFields)
      println(parsed.getField("svse258NoticeKey"))

      val genRec1 = new GenericData.Record(parsed.getField("svse258NoticeKey").schema())
      println(genRec1.getSchema.toString(true))
      println(genRec1.getSchema.getFields)
      genRec1.put("svse258DueDate", 3L)
      genRec1.put("svse258SeqNbr", 3L)

      genRec.put("svse258NoticeKey", genRec1)

      val genRec2 = new GenericData.Record(parsed.getField("svse258NoticeDetail").schema())
      println(genRec2.getSchema.toString(true))
      println(genRec2.getSchema.getFields)
      genRec2.put("svse258ReqstAmt", 3.0000000333)
      genRec2.put("svse258ActlAmt", 3.234235)
      genRec2.put("svse258FateOfFundsCode", 3L)
      genRec2.put("svse258PnltAmt", 3.12341243)
      genRec2.put("svse258PnltPct", 3.12341243)
      genRec2.put("svse258ReqstAvailWdAmt", 3.12341243)
      genRec2.put("svse258OrigActlAvailWdAmt", 3.12341243)
      genRec2.put("svse258ActlAvailWdAmt", 3.12341243)
      genRec2.put("svse258ActlAvailInstrAmt", 3.12341243)
      genRec2.put("svse258NoticeStatCode", "SDFADF")
      genRec2.put("svse258NoticeType", "SDFADF")
      genRec2.put("svse258EarlyWthoutPnltDate", 3L)
      genRec2.put("svse258EarlyWithPnltDate", 3L)
      genRec2.put("svse258LatestDueDate", 3L)
      genRec2.put("svse258ErWoPnltDteSeqNbr", 3L)
      genRec2.put("svse258ErWtPnltDteSeqNbr", 3L)
      genRec2.put("svse258LatestDueDteSeqNbr", 3L)
      genRec2.put("svse258CaptDate", 3L)
      genRec2.put("svse258GracePerExpyDate", 3L)
      genRec2.put("svse258MaxWdNoticeNbr", 3L)
      genRec2.put("svse258ExtSrceCode", "laaitie")

      genRec2.put("svse258TransSiteCode", 3L)
      genRec2.put("svse258ShortNoticeInd", "afadf")
      genRec2.put("svse258CalcPnltAmt", 2344.435435)
      genRec2.put("svse258AcctName", "adfaf")
      genRec2.put("svse258UpfPnltAmnt", 3.455533)

      genRec.put("svse258NoticeDetail", genRec2)

      println(Console.CYAN + "GENREC : " + genRec.toString + Console.WHITE)

      val datumWriter = new GenericDatumWriter[GenericRecord](parsed)
      val dataFileWriter = new DataFileWriter[GenericRecord](datumWriter)
      dataFileWriter.create(parsed, new File("src/test/resources/Svse258NoticeRecord.txt"))
      dataFileWriter.append(genRec)
      dataFileWriter.close()
    }

    "deserialize a serialized txt file back to generic Record and create instance of case class" in {
      println(Console.GREEN + "deserialize a txt file back to generic Record and create instance of case class" + Console.WHITE)
      //      val genRec = RecordFormat[Svse258NoticeRecord]
      val parsed = new Schema.Parser().parse(new File("src/test/resources/Svse258NoticeRecord.avsc"))
      val datumReader = new GenericDatumReader[GenericRecord](parsed)
      val dataFileReader = new DataFileReader[GenericRecord](new File("src/test/resources/Svse258NoticeRecord.txt"), datumReader)

      while (dataFileReader.hasNext()) {
        val genRec = dataFileReader.next()
        println(genRec)
        val format = RecordFormat[Svse258NoticeRecord]
        val noticeRec = format.from(genRec)
        println("svse258NoticeRecord.svse258NoticeDetail : " + noticeRec.svse258NoticeDetail)
      }
    }

    "parse raw data file to generic record" in {
      println(Console.GREEN + "parse raw data file to generic record" + Console.WHITE)

      val source: BufferedSource = Source.fromFile("src/test/resources/SVSE258.txt")
      val lines: String = try source.getLines().mkString("\n") finally source.close()
      val roots: Seq[Group] = CopyBookSchema(lines).parseTree(ASCII())
      println("Roots :  " + roots.mkString("\n"))
      val parsed = new Schema.Parser().parse(new File("src/test/resources/Svse258NoticeRecord.avsc"))
      val bytes = Files.copyBytes("src/test/resources/test_dump_bin")
      println(s"Bitvector of input file ${bytes.toBin}")
      val genRecBuilder = Files.rawDataParse(bytes, parsed, roots)
      println("Generic record : ")
      genRecBuilder.foreach({
        rec =>
          println("newGenRec : " + rec.toString)
      })
    }
  }

  /* "For multible copybooks the test" should {
    txtFiles.foreach(fileName => {
      val source: BufferedSource = Source.fromFile(fileName)
      val lines: String = try source.getLines().mkString("\n") finally source.close()
      //    //println(s"lines is $lines")
      val roots: Seq[Group] = CopyBookSchema(lines).parseTree()

      println("ROOTS : " + roots.mkString("//"))

      s"create case classes for each AST : $fileName" in {
        val packageName = "com.zenaptix.test"
        roots.foreach { root =>
          println(root.name)
          val c = root.traverseGroups.map(g => g.asCaseClass)
          //        println("Case Classes : ")
          //        c.foreach(g => println(g))
          root.printToFile(new File(s"src/test/scala/${packageName.replace(".", "/")}/${root.camelCased}.scala")) { p =>
            p.println(s"package $packageName")
            c.foreach(p.println)
          }
        }
      }

      s"create an AVRO schema for each AST : $fileName" in {
        import com.zenaptix.test._
        roots.foreach { root =>
          val schema = AvroSchema[root.type]
          root.printToFile(new File(s"src/test/resources/${root.camelCased}.avsc")) { p =>
            p.println(schema.toString(true))
          }
          println(schema)
        }
      }
    })
  }
  */
}
