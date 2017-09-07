import com.sksamuel.avro4s.{AvroSchema, RecordFormat}
import com.zenaptix.dsl._
import com.zenaptix.dsl.Files._
import com.zenaptix.test.Cqsf602w
import org.apache.avro.Schema
import org.apache.avro.Schema.Field
import org.apache.avro.generic.{GenericData, GenericRecord, GenericRecordBuilder}
import scodec.bits.BitVector
import shapeless.{::, HList, HNil}

import scala.io.{BufferedSource, Source}

-1.toInt match {
  case 0x01 => "yes"
  case _ => "No"
}

val source: BufferedSource = Source.fromFile("/home/rikus/Downloads/mainframe_test/CQSF602.txt")
val lines: String = try source.getLines().mkString("\n") finally source.close()
val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")

println("FROM BIN FILE 0-464: " + bytes.slice(0,464).toBin)
println("FROM BIN FILE 448-1000: " + bytes.slice(448,1000).toBin)

val combined = s"${bytes.slice(0,32).toBin}${bytes.slice(32,66).toBin}${bytes.slice(66,99).toBin}"
println(combined)
assert(combined == bytes.slice(0,99).toBin)
//create tree
val forest: Seq[Group] = CopyBookSchema(lines).parseTree(EBCDIC())
val roots = forest.head.traverseAll
val schema: Schema = AvroSchema[Cqsf602w]
val origRec = new GenericData.Record(schema)
println("origRec fields : " + origRec.getSchema.getFields)
val itr = origRec.getSchema.getFields.iterator()
while (itr.hasNext) {
  println(itr.next().schema())
}

val genRecValues = Files.rawDataList(0,bytes, schema, forest)
val genRecVal = genRecValues.head.filter(hlst => hlst match {
  case head :: HNil => true
  case _ => false
})


val root = roots.head
val finalRec = recursiveBuilder(root, roots, origRec, genRecVal.toIterator)
println(finalRec.toString)
//val format = RecordFormat[Cqsf602w]
//val newCaseClass = format.from(finalRec)
//println(finalRec.getSchema.toString(true))
//forest.foreach(tree => {
//  val roots = tree.traverseAll
//  roots.foreach(root => {
//    println("TEST ROOT - " + root.camelCaseVar)
//    val finalRec = recursiveBuilder(root, roots, origRec, genRecVal.toIterator)
//    println(finalRec.toString)
//    //    val correctSchema = (getGenRec(origRec,root,roots)).getOrElse(origRec)
//    //    println("correctSchema : " + correctSchema.getSchema.getFields)
//  })
//
//})

