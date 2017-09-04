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
import scala.reflect.api
import scala.util.{Failure, Success, Try}
import scala.reflect.runtime.universe._

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

def getScalaType(typeString: String) = {
  typeString match {
    case "string" => Class.forName(s"java.lang.${typeString.capitalize}")
    case _ => Class.forName(s"scala.${typeString.capitalize}")
  }
}
def stringToTypeTag[A](name: String): TypeTag[A] = {
  val c = name match {
    case "string" => Class.forName(s"java.lang.${name.capitalize}")
    case _ => Class.forName(s"scala.${name.capitalize}")
  }
  val mirror = runtimeMirror(c.getClassLoader) // obtain runtime mirror
  val sym = mirror.staticClass(c.getName) // obtain class symbol for `c`
  val tpe = sym.selfType // obtain type object for `c`
  // create a type tag which contains above type object
  TypeTag(mirror, new api.TypeCreator {
    def apply[U <: api.Universe with Singleton](m: api.Mirror[U]) =
      if (m eq mirror) tpe.asInstanceOf[U#Type]
      else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
  })
}
def cast[A](a: Any, tt: TypeTag[A]): A = a.asInstanceOf[A]


def recursiveBuilder(root: CBTree, roots: Seq[CBTree], origRec: GenericData.Record, values: Iterator[HList]): GenericData.Record = {
  val fields = origRec.getSchema.getFields
  println("FIELDS REC : " + fields)
  val fieldsList = fields.toArray.toList.map(field => {
    field.toString.split(" ").toList.head
  })
  println("fields List : " + fieldsList)
  var newRec = origRec
  if (fields.size() > 0) { //root has child
    fieldsList.foreach(fieldName => {
      println("FOR EACH CHILD FIELD : " + fieldName)
      val childField = origRec.getSchema.getField(fieldName)
      println("origRec.getSchema.getField(fieldName)/childField " + childField)
      println("origRec.getSchema.getField(fieldName).schema/childField.schema : " + childField.schema())
      //      println("origRec.fields : " + childField.schema().getFields)
      val isLeaf = Try {
        childField.schema().getFields
      } match {
        case Success(ans) =>
          val newFieldRec = new GenericData.Record(childField.schema())
          newRec = recursiveBuilder(root, roots, newFieldRec, values)
          false
        case Failure(e) =>
          println("ERROR : " + e)
          println("else origRec.getSchema.getName " + origRec.getSchema.getName)
          println("Put " + fieldName + " IN " + origRec.getSchema.getName)
          val fieldVal = values.next() match {
            case h :: HNil => h
            case _ => println("&&&!!!!!")
          }
          val fieldType = origRec.getSchema.getField(fieldName).schema().getType
          val stringToType = stringToTypeTag(fieldType.getName)
          origRec.put(fieldName, cast(fieldVal,stringToType))
          println("origRec put : " + origRec.toString)
          true
      }
      // if the current root is a leaf(i.e. no children) then dont do group put
      if(!isLeaf){
        println("group put " + fieldName + " IN " + origRec.getSchema.getName)
        println(s"origRec.put &&&& (${fieldName} , ${childField.toString})")
//        origRec.put(fieldName, childField)
//        origRec.put(fieldName, new GenericData.Record(childField.schema()))
        println("NEW REC :  " + newRec.toString)
//        origRec.put(fieldName, new GenericData.Record(origRec.getSchema.getField(fieldName).schema()))
        origRec.put(fieldName, newRec)
        println("origRec put : " + origRec.toString)
      }
    })
    println("origRec IF before exit" + origRec.toString)
    origRec
  }
  else {
    println("Put " + root.camelCaseVar + " IN " + origRec.getSchema.getName)
    val fieldVal = values.next() match {
      case h :: HNil => h
      case _ => println("&&&!!!!!")
    }
    val fieldType = origRec.getSchema.getField(root.camelCaseVar).schema().getType
    val stringToType = stringToTypeTag(fieldType.getName)
    println("FIELD VAL !!!! ELSE : " + fieldVal.toString)
    origRec.put(root.camelCaseVar, cast(fieldVal,stringToType))
    println("origRec put ELSE : " + origRec.toString)
    origRec
  }
}
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
