package com.zenaptix.dsl

import java.io.File

import org.scalatest.WordSpec

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import com.sksamuel.avro4s._
import org.apache.avro.Schema
import org.apache.avro.file.{DataFileReader, DataFileWriter}
import org.apache.avro.generic.{GenericData, GenericDatumReader, GenericDatumWriter, GenericRecord}
import org.apache.avro.specific.SpecificDatumWriter

/**
 * Created by ian on 2017/01/21.
 */

case class Composer(name: String, birthplace: String, compositions: Seq[String])

class CopyBookSchemaSpec extends WordSpec {

  val cpyBook: String =
    """      01   PKLR1-DETAIL-LOAN-RECORD.
                  |      10  PKLR1-BASIC-SECTION.
                  |      * 10  PKLR1-BASIC-SECTION.
                  |      * 10  PKLR1-BASIC-SECTION.
                  |       20  PKLR1-SORT-CONTROL-FIELD.
                  |           30  PKLR1-USER-IDENT         PIC X(1).
                  |           30  PKLR1-EXTRACT-CODE
                  |               PIC X(1).
                  |               88  PKLR1-DATE-RECORD            VALUE '*'.
                  |               88  PKLR1-DATA-RECORD            VALUE '0'.
                  |               88  PKLR1-END-OF-FILE            VALUE '9'.
                  |           30  PKLR1-SECTION            PIC X(1).
                  |           30  PKLR1-TYPE               PIC X(1).
                  |           30  PKLR1-NUMERIC-STATE-CODE PIC X(2).
                  |           30  PKLR1-CONTRACT-NUMBER    PIC X(10).
                  |       20  PKLR1-PAR-PEN-REG-CODE       PIC X(1).
                  |       20  PKLR1-VALUATION-CODE.
                  |           30  PKLR1-MORTALITY-TABLE    PIC X(2).
                  |           30  PKLR1-LIVES-CODE         PIC X(1).
                  |           30  PKLR1-FUNCTION           PIC X(1).
                  |           30  PKLR1-VAL-INTEREST       PIC S9(2)V9(3) COMP-3.
                  |           30  PKLR1-MODIFICATION       PIC X(1).
                  |           30  PKLR1-INSURANCE-CLASS    PIC X(1).
                  |           30  PKLR1-SERIES             PIC X(5).
                  |       20  PKLR1-POLICY-STATUS          PIC X(2).
                  |       20  PKLR1-PAR-CODES.
                  |           30  PKLR1-PAR-TYPE           PIC X(1).
                  |           30  PKLR1-DIVIDEND-OPTION    PIC X(1).
                  |           30  PKLR1-OTHER-OPTION       PIC X(1).
                  |       20  PKLR1-ALPHA-STATE-CODE       PIC X(2).""".stripMargin

  val cpyBook2: String =
    """      01   PKLR1-DETAIL-LOAN-RECORD.
      |      10  PKLR1-BASIC-SECTION.
      |      * 10  PKLR1-BASIC-SECTION.
      |      * 10  PKLR1-BASIC-SECTION.
      |       20  PKLR1-SORT-CONTROL-FIELD.
      |           30  PKLR1-USER-IDENT         PIC X(1).
      |           30  PKLR1-EXTRACT-CODE
      |               PIC X(1).
      |               88  PKLR1-DATE-RECORD            VALUE '*'.
      |               88  PKLR1-DATA-RECORD            VALUE '0'.
      |               88  PKLR1-END-OF-FILE            VALUE '9'.
      |           30  PKLR1-SECTION            PIC X(1).
      |           30  PKLR1-TYPE               PIC X(1).
      |           30  PKLR1-NUMERIC-STATE-CODE PIC X(2).
      |           30  PKLR1-CONTRACT-NUMBER    PIC X(10).
      |       20  PKLR1-PAR-PEN-REG-CODE       PIC X(1).
      |       20  PKLR1-VALUATION-CODE.
      |           30  PKLR1-MORTALITY-TABLE    PIC X(2).
      |           30  PKLR1-LIVES-CODE         PIC X(1).
      |           30  PKLR1-FUNCTION           PIC X(1).
      |           30  PKLR1-VAL-INTEREST       PIC S9(2)V9(3) COMP-3.
      |           30  PKLR1-MODIFICATION       PIC X(1).
      |           30  PKLR1-INSURANCE-CLASS    PIC X(1).
      |           30  PKLR1-SERIES             PIC X(5).
      |       20  PKLR1-POLICY-STATUS          PIC X(2).
      |       20  PKLR1-PAR-CODES.
      |           30  PKLR1-PAR-TYPE           PIC X(1).
      |           30  PKLR1-DIVIDEND-OPTION    PIC X(1).
      |           30  PKLR1-OTHER-OPTION       PIC X(1).
      |       20  PKLR1-ALPHA-STATE-CODE       PIC X(2).
      |      01   PKLR1-DETAIL-LOAN-RECORD-COPY.
      |      10  PKLR1-BASIC-SECTION-COPY.
      |       20  PKLR1-SORT-CONTROL-FIELD-COPY.
      |           30  PKLR1-USER-IDENT-COPY         PIC X(1).
      |       """.stripMargin

  val s1: String = """case class PKLR1DetailLoanRecord(val pKLR1SortControlField: PKLR1SortControlField)"""

  //val source: BufferedSource = Source.fromFile("src/test/resources/Cheques_Master.txt")
  val source: BufferedSource = Source.fromFile("src/test/resources/SVSE258.txt")
  val lines: String = try source.getLines().mkString("\n") finally source.close()

  //println(s"lines is $lines")

  val roots: Seq[Group] = CopyBookSchema(lines).parseTree(ASCII())
  //  val roots: Seq[Group] = CopyBookSchema(cpyBook).parseTree()
  println("Roots :  " + roots.mkString("\n"))

  val filesDir = Files.getListOfFiles("./src/test/resources")
  val txtFiles = filesDir.filter(file => file.toString.contains(".txt")).map(file => file.toString)

  "CopyBookSchema" should {

    "build a navigable AST" in {
      val tree = Group(1, "root", mutable.ArrayBuffer(), parent = None)
      val node = tree
        .add(Group(10, "test", mutable.ArrayBuffer(), parent = None))
        .asInstanceOf[Group]
      node.add(Group(20, "blah", mutable.ArrayBuffer(), parent = None))
      val ll1 = node.add(Statement(20, "l1", parent = None, dataType = AlphaNumeric(length = 1)))
      val ll2 = node.add(Statement(20, "l2", parent = None, dataType = AlphaNumeric(length = 2)))
      val ll3 = node.add(Statement(20, "l3", parent = None, dataType = AlphaNumeric(length = 3)))
      val ll4 = node.add(Statement(20, "l4", parent = None, dataType = AlphaNumeric(length = 4)))
      node.add(Group(20, "b1", mutable.ArrayBuffer(), parent = None))
      node.add(Group(20, "b2", mutable.ArrayBuffer(), parent = None))

      val path = tree.children.head.asInstanceOf[Group].children.head.path()
      println(s"path: $path")
      val x = tree.get(path)
      println(x)
      val y = tree.get("root/test/blah")
      println(s"get - $y")
      val z = tree.get("test/blah")
      println(s"get - $z")
      val e = tree.get("root/tst/blah")
      println(s"get - $e")
      val d = node.up()
      println(d)
      val f = tree.up()
      println(f)
      println(s"\n\n path ---- ${ll2.path()}")

      val l2 = tree.get("/root/test/l2")
      println(s"l2 ---- $l2")

      println(s"l3 ---- ${ll2.nextSiblingLeaf()}")
      println(s"l3 ---- ${tree.get("/root/test/l3")}")

      println(s"l4 ---- ${ll3.nextSiblingLeaf()}")
      println(s"l4 ---- ${tree.get("/root/test/l4")}")

      println(s"none ---- ${ll4.nextSiblingLeaf()}")
      println(s"none ---- ${tree.get("/root/test/l5")}")

    }

    "traverse the AST" in {
      roots.foreach { root =>
        println("Root : " + root.name)
        val s = root.traverseStatements
        println(s"\n\nTRAVERSAL 1 {Statements}:\n ${s.mkString("\n")}")

        val t = root.traverseAll
        println(s"\n\nTRAVERSAL 2 {All}:\n ${t.mkString("\n")}")

        val g = root.traverseGroups
        println(s"\n\nTRAVERSAL 3 {Groups}:\n ${g.mkString("\n")}")
      }
    }

    "traverse the statements of an AST " in {
      roots.foreach({ root =>
        root.traverseStatements.map({ statement =>
          statement.name
        })
      })
    }

    "camelCase a variable name and" in {
      roots.foreach { root =>
        println(s"${root.name} camelCased is ${root.camelCased} ->  ${root.camelCaseVar} ")
      }
    }

    "parse data using CBTree" in {
      import scodec._
      import scodec.bits._
      import codecs._
      val y: BitVector = BitVector("1234testing".getBytes) ++ BitVector(0x10, 0x2a, 0x03, 0xff) ++ BitVector("5678MoreTesting".getBytes)
      println("bitVector : " + y)
      val firstCodec = uint4 ~ uint4 ~ uint16
      val result2: Attempt[DecodeResult[(Int ~ Int ~ Int)]] = Codec.decode(y)(firstCodec)

      println("Decoded Result : " + result2)
      println(result2.require.remainder.decodeAscii.right)
      println(result2.require.value)

      println(y)

    }

    "create case classes source from AST" in {
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

    "create AVRO schema from AST" in {
      import com.zenaptix.test._
      roots.foreach { root =>
        val schema = AvroSchema[Svse258NoticeRecord]
        root.printToFile(new File(s"src/test/resources/${root.camelCased}.avsc")) { p =>
          p.println(schema.toString(true))
        }
        println(schema)
      }
    }

    "parse AVRO schema from file and serialize a generic record" in {
      import com.zenaptix.test._
      val parsed = new Schema.Parser().parse(new File("src/test/resources/Svse258NoticeRecord.avsc"))
      println(parsed)
      println(parsed.getField("svse258RecLength"))
      val genRec = new GenericData.Record(parsed)
      genRec.put("svse258RecLength", 3)
      println("Fields : " + parsed.getFields)
      println(parsed.getField("svse258NoticeKey"))

      val genRec1 = new GenericData.Record(parsed.getField("svse258NoticeKey").schema())
      println(genRec1.getSchema)
      println(genRec1.getSchema.getFields)
      genRec1.put("svse258DueDate", 3L)
      genRec1.put("svse258SeqNbr", 3L)

      genRec.put("svse258NoticeKey", genRec1)

      val genRec2 = new GenericData.Record(parsed.getField("svse258NoticeDetail").schema())
      println(genRec2.getSchema)
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

      println("GENREC : " + genRec.toString)

      val datumWriter = new GenericDatumWriter[GenericRecord](parsed)
      val dataFileWriter = new DataFileWriter[GenericRecord](datumWriter)
      dataFileWriter.create(parsed, new File("src/test/resources/Svse258NoticeRecord.txt"))
      dataFileWriter.append(genRec)
      dataFileWriter.close()
    }

    "deserialize a txt file back to generic Record and create instance of case class" in {
      import com.zenaptix.test._
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
      import scodec._
      import scodec.bits._
      import codecs._

      val source: BufferedSource = Source.fromFile("src/test/resources/SVSE258.txt")
      val lines: String = try source.getLines().mkString("\n") finally source.close()
      val roots: Seq[Group] = CopyBookSchema(lines).parseTree(ASCII())
      println("Roots :  " + roots.mkString("\n"))
      val parsed = new Schema.Parser().parse(new File("src/test/resources/Svse258NoticeRecord.avsc"))
      val bytes = Files.copyBytes("src/test/resources/test_dump_bin")
      bytes.toBin
      val genRecBuilder = Files.rawDataParse(bytes, parsed, roots)
      println("Generic record : ")
      genRecBuilder.foreach({ rec =>
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

//      s"create an AVRO schema for each AST : $fileName" in {
//        import com.zenaptix.test._
//        roots.foreach { root =>
//          val schema = AvroSchema[root.type]
//          root.printToFile(new File(s"src/test/resources/${root.camelCased}.avsc")) { p =>
//            p.println(schema.toString(true))
//          }
//          println(schema)
//        }
//      }

    })
  }
  */
}
