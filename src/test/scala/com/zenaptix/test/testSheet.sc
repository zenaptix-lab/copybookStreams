import com.sksamuel.avro4s.AvroSchema
import com.zenaptix.dsl._
import com.zenaptix.dsl.Files._
import com.zenaptix.test.Cqsf602w
import org.apache.avro.Schema
import org.apache.avro.Schema.Field
import org.apache.avro.generic.{GenericData, GenericRecord, GenericRecordBuilder}
import scodec.bits.BitVector
import shapeless.{::, HList, HNil}

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

val source: BufferedSource = Source.fromFile("/home/rikus/Downloads/mainframe_test/CQSF602.txt")
val lines: String = try source.getLines().mkString("\n") finally source.close()
val bytes: BitVector = Files.copyBytes("/home/rikus/Downloads/mainframe_test/PCHEQ.WWORK.IMSP.CQSF602.DATA.AUG07")

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

val genRecValues = Files.rawDataList(bytes, schema, forest)
val genRecVal = genRecValues.head.filter(hlst => hlst match {
  case head :: HNil => true
  case _ => false
})

def recursiveBuilder(root: CBTree, roots: Seq[CBTree], origRec: GenericData.Record, values: Iterator[HList]): GenericData.Record = {
  val fields = origRec.getSchema.getFields
  println("FIELDS REC : " + fields)
  val fieldsList = fields.toArray.toList.map(field => {
    field.toString.split(" ").toList.head
  })
  println("fields List : " + fieldsList)
  if (fields.size() > 0) { //root has child
    fieldsList.foreach(fieldName => {
      val childField = origRec.getSchema.getField(fieldName)
      println("origRec.getSchema.getField(fieldName): " + childField)
      println("origRec.getSchema.getField(fieldName).schema : " + childField.schema())
      //      println("origRec.fields : " + childField.schema().getFields)
      Try {
        childField.schema().getFields
      } match {
        case Success(ans) =>
          val newFieldRec = new GenericData.Record(childField.schema())
          recursiveBuilder(root, roots, newFieldRec, values)
        case Failure(e) =>
          println("ERROR : " + e)
          println("else return " + origRec.getSchema.getName)
          println("Put " + fieldName + " IN " + origRec.getSchema.getName)
          origRec.put(fieldName, values.next())
          println("origRec put : " + origRec.toString)
      }
    })
    origRec
  }
  else {
    println("Put " + root.camelCaseVar + " IN " + origRec.getSchema.getName)
    origRec.put(root.camelCaseVar, values.next())
    println("origRec put ELSE : " + origRec.toString)
    origRec
  }
}
val root = roots.head
val finalRec = recursiveBuilder(root, roots, origRec, genRecVal.toIterator)
println(finalRec.toString)
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
